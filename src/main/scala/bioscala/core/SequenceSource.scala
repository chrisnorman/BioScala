/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.core

import scala.annotation.tailrec
import scala.util.{ Try, Success, Failure }

import bioscala.gentypes._
import bioscala.filehandlers.FASTAFileSource

/**
 * A source of sequence data (i.e., in-memory strings, FASTA files, or cached) which can be
 * processed either by enumerator/iteratee pairs or by iteration.
 *
 */
trait SequenceSource
  extends Enumerator[Char]
  with Traversable[Char]
  with Iterable[Char]
{

  def apply(i: Int): Try[Char]
  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R]
  override def foreach[U](f: Char => U) = enumerate(Iteratee.fold[Char, Unit](Unit)((r, e) => f(e)))
  override def iterator: Iterator[Char]
  def reify: Try[SequenceSource]
  def reverse: Try[SequenceSource]
  private final val maxChars = 20

  override def toString: String = getSequenceString(Some(maxChars))

  /*
   * Get a string representing the sequence string; if nChars == None, the string will
   * contain the entire sequence (which could be LARGE), otherwise defaults to 20 chars.
   */
  def getSequenceString(nChars: Option[Long] = None): String = {
    val sb: StringBuffer = new StringBuffer;
    def getChars(n: Long): Iteratee[Char, String] = {
      def step(sbuf: StringBuffer, count: Long): Input[Char] => Iteratee[Char, String] = {
        case Element(e) =>  if (n == -1 || count < n) Continue(step(sbuf.append(e), count + 1))
        					else Done(sbuf.toString, Element(e))
        case EndOfInput => Done(sbuf.toString, EndOfInput)
      }
      Continue(step(sb, 1))
    }
    enumerate(getChars(nChars.getOrElse(-1))).result match {
      case Success(s) => s
      case Failure(t) => "getChars failed: " + t.getMessage
    }
  }
}

/**
 * Sequence source backed by an in-memory string.
 */
class SequenceSourceString(val seqStr: String) extends SequenceSource
{

  override def apply(i: Int): Try[Char] = Try(seqStr(i))
  override def foreach[U](f: Char => U) = seqStr.foreach(f)
  override def iterator: Iterator[Char] = seqStr.iterator
  override def reify = Try(this)

  /*
   * Obtain a sequence representing the reverse of this sequence. Force the sequence into
   * memory so we can use a reverse cache.
   */
  override def reverse: Try[SequenceSource] = {
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield src
  }

  protected final def enumerateStep[R](itr: Iterator[Char], it: Iteratee[Char, R]): Iteratee[Char, R] = {
    @tailrec
    def eStep(ite: Iteratee[Char, R]): Iteratee[Char, R] = {
      ite match {
        case Continue(f) =>
          if (itr.hasNext) eStep(f(Element(itr.next)))
          else eStep(f(EndOfInput))
        case other @_ => other
      }
    }
    eStep(it)
  }

  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = enumerateStep(seqStr.iterator, it)
}

/**
 * Sequence backed by a FASTA file; only the first sequence in the file is represented
 * (a multi-sequence FAST file can be read with FASTAFileReader).
 * 
 * NOTE that in order to reverse, access by index, or iterate over a FASTA File SequenceSource
 * it must first be reified into memory via a cached source (see below).
 */
class SequenceSourceFASTA(fileName: String) extends SequenceSource {

  override def apply(i: Int): Try[Char] =
    Failure(new IllegalStateException("FASTA File source must be reified for random access"))
  /*
   *  Note: this iterates over a temporarily reified Source, which is inefficient. Unless
   *  this is a one-shot iteration, it would be better for the user to reify it into a sequence
   *  first and then subsequently use that.
   */
  override def iterator = this.reify.map(s => s.iterator).get

  override def reify: Try[SequenceSource] = {
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator).result
      cache <- tryCache
      src = new SequenceSourceCache(cache)
    } yield src
  }

  override def reverse: Try[SequenceSource] = {
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield src
  }

  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = {
    val ffSource = new FASTAFileSource(fileName)
    ffSource.enumerate(it)
  }
}

/**
 * Source backed by another source that is (lazily) transformed via a 1:1 transformation
 * function. (For example, this source might represent an RNA sequence which is transformed
 * from a DNA sequence via a transcription function). The original (DNA sequence) source is
 * maintained as the source, and the enumerator just "lifts" any supplied Iteratee so
 * that the step function's input is transformed on demand. 
 */
class SequenceSourceMappedLinear(val src: SequenceSource, transform: Char => Char) extends SequenceSource {
  override def apply(i: Int): Try[Char] = src(i).map(transform)
  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = src.enumerate(it.mapInput(transform))
  override def foreach[U](f: Char => U) = enumerate(Iteratee.fold[Char, Unit](Unit)((r, e) => f(e)))
  override def iterator: Iterator[Char] = src.iterator.map(transform)
  override def reify = src.reify.map(s => new SequenceSourceMappedLinear(s, transform))
  override def reverse: Try[SequenceSourceMappedLinear] = src.reverse.map(s => new SequenceSourceMappedLinear(s, transform))
}

/**
 * Source backed by an in-memory cache, which may be packed (2 bits/char for DNA)
 * or unpacked.
 */
class SequenceSourceCache(val cache: SequenceCache) extends SequenceSource {
  override def apply(i: Int): Try[Char] = cache.apply(i)
  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = cache.enumerate(it)
  override def iterator: Iterator[Char] = new Iterator[Char] {
    var i = 0
    def hasNext: Boolean = i < cache.length
    def next: Char = {val ret = cache(i); i+=1; ret.get}
  }
  override def reify = Try(this)
  override def reverse: Try[SequenceSourceReverseCache] = Try(new SequenceSourceReverseCache(cache))
}

/**
 * Source backed by an-in memory cache that is stored in reverse order. This allows
 * a single cache which has been reified from, say, a FASTA file, to be the shared representation
 * for the Sequences that represent the original, forward sequence; the reverse of that sequence;
 * and via SequenceSourceMappedLinear, the complement or transcription of the reverse sequence;
 * without ever actually materializing the reverse, complement, or transcribed sequences in memory.
 */
class SequenceSourceReverseCache(val cache: SequenceCache) extends SequenceSource {
  override def apply(i: Int): Try[Char] = cache.apply(cache.length - 1 - i)
  override def reify = Try(this)
  override def reverse: Try[SequenceSourceCache] = Try(new SequenceSourceCache(cache))
  override def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = cache.enumerateReverse(it)
  override def iterator: Iterator[Char] = new Iterator[Char] {
    var i = 0
    def hasNext: Boolean = i < cache.length
    def next: Char = {val ret = cache(cache.length - 1 - i); i+=1; ret.get}
  }
}

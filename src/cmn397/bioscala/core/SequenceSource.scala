/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.core

import scala.annotation.tailrec
import scala.util.{ Try, Success, Failure }

import cmn397.bioscala.gentypes._
import cmn397.bioscala.filehandlers.FASTAFileSource

/**
 * Base trait for representing a source of sequence data (i.e., in-memory strings or FASTA files)
 * which can be processed by enumerator/iteratee pairs.
 *
 */
trait SequenceSource extends Enumerator[Char] {
  
  def apply(i: Int): Try[Char]
  def reify: Try[SequenceSource]
  def reverse: Try[SequenceSource]
  private final val maxChars = 20
  override def toString: String = getSequenceString(Some(maxChars))
  def getSequenceString(nChars: Option[Long] = None): String = {
    val sb: StringBuffer = new StringBuffer;
    def getChars(n: Long): Iteratee[Char, String] = {
      def step(sbuf: StringBuffer, count: Long): Input[Char] => Iteratee[Char, String] = {
        case Element(e) => if (n == -1 || count < n) Continue(step(sbuf.append(e), count + 1))
        				   else Done(sbuf.toString, Pending)
        case Pending => Done(sbuf.toString, Pending)
        case EndOfInput => Done(sbuf.toString, EndOfInput)
      }
      Continue(step(sb, 1))
    }
    // if nChars == None, then enumerate all characters
    enumerate(getChars(nChars.getOrElse(-1))).result match {
      case Success(s) => s
      case Failure(t) => "getChars failed: " + t.getMessage
    }
  }

}

class SequenceSourceString(val seqStr: String) extends SequenceSource {

  override def apply(i: Int): Try[Char] = Try(seqStr(i))
  override def reify = Try(this)
  override def reverse: Try[SequenceSource] = {
    // force this into memory so we can use a reverse cache; presumably, since the source is a string,
    // its not that big anyway..
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield src
  }

  protected final def enumerateStep[R](itr: Iterator[Char]): Iteratee[Char, R] => Iteratee[Char, R] = {
    @tailrec
    def eStep(ite: Iteratee[Char, R]): Iteratee[Char, R] = {
      ite match {
        case Continue(f) =>
          if (itr.hasNext) eStep(f(Element(itr.next)))
          else eStep(f(EndOfInput))
        case other @_ => other
      }
    }
    eStep
  }

  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = enumerateStep(seqStr.iterator)
}

/*
 * This source is backed by a FASTA file. Note that in order to reverse or access a FASTA File SequenceSource
 * by index, it must first reified into memory via a cached source (see below).
 */
class SequenceSourceFASTA(fileName: String) extends SequenceSource {

  override def apply(i: Int): Try[Char] = Failure(new IllegalStateException("FASTA File source must be reified for random access"))
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

  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = {
    val ffSource = new FASTAFileSource(fileName)
    ffSource.enumerate
  }
}

/*
 * This source is backed by another source that is (lazily) transformed via a 1:1 transformation
 * function (ie, this might represent an RNA sequence which is transformed from a DNA sequence via
 * a transcription function). The original (DNA sequence) source is maintained as the source, and
 * the enumerator just "lifts" any supplied Iteratee so that the step function's input is transformed
 * on demand.
 *  
 */
class SequenceSourceMappedLinear(val src: SequenceSource, transform: Char => Char) extends SequenceSource {
  override def apply(i: Int): Try[Char] = src(i).map(transform)
  override def reify = src.reify.map(s => new SequenceSourceMappedLinear(s, transform))
  override def reverse: Try[SequenceSourceMappedLinear] = src.reverse.map(s => new SequenceSourceMappedLinear(s, transform))
  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = ite => src.enumerate(ite.mapInput(transform))
}

/*
 * This source is backed by an in memory cache, which may be packed (2 bits/char for DNA)
 * or unpacked.
 */
class SequenceSourceCache(val cache: SequenceCache) extends SequenceSource {
  override def apply(i: Int): Try[Char] = Try(cache.valueAt(i))
  override def reify = Try(this)
  override def reverse: Try[SequenceSourceReverseCache] = Try(new SequenceSourceReverseCache(cache))
  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = cache.enumerate
}

/*
 * This source is backed by an in memory cache that is stored in reverse order. This allows
 * a single cache which has been reified from, say, a FASTA file, to be the shared representation
 * for the Sequences that represent the original, forward sequence; the reverse of that sequence;
 * and via SequenceSourceMappedLinear, the complement or transcription of the reverse sequence,
 * without ever actually materializing the reverse, complement, or transcribed sequences in memory.
 */
class SequenceSourceReverseCache(val cache: SequenceCache) extends SequenceSource {
  override def apply(i: Int): Try[Char] = Try(cache.valueAt(cache.length - 1 - i))
  override def reify = Try(this)
  override def reverse: Try[SequenceSourceCache] = Try(new SequenceSourceCache(cache))
  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = cache.enumerateReverse
}

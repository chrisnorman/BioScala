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

  private final val maxChars = 20
  override def toString: String = getSequenceString(Some(maxChars))
}

class SequenceSourceString(val seqStr: String) extends SequenceSource {

  def apply(i: Int): Try[Char] = Try(seqStr(i))

  override def reify = Try(this)

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

class SequenceSourceFASTA(fileName: String) extends SequenceSource {

  def apply(i: Int): Try[Char] = Failure(new IllegalStateException("Must reify FASTA File source"))

  override def reify: Try[SequenceSource] = {
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator).result
      cache <- tryCache
      src = new SequenceSourceCache(cache)
    } yield src
  }

  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = {
    val ffSource = new FASTAFileSource(fileName)
    ffSource.enumerate
  }
}

/*
 * This source is represented by another source that is (lazily) transformed via a 1:1 transformation
 * function (ie, this might represent an RNA sequence which is transformed from a DNA sequence via
 * a transcription function). The original (DNA sequence) source is maintained as the source, and
 * the enumerator just "lifts" any supplied Iteratee so that the step function's input is transformed
 * on demand.
 *  
 */
class SequenceSourceMappedLinear(val src: SequenceSource, transform: Char => Char) extends SequenceSource {

  def apply(i: Int): Try[Char] = Failure(new IllegalStateException("Must reify FASTA File source"))

  override def reify: Try[SequenceSource] = {
    for {
      tryCache <- enumerate(SequenceCache.packedCacheGenerator.mapInput(transform)).result
      cache <- tryCache
      src = new SequenceSourceCache(cache)
    } yield src
  }

  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = ite => src.enumerate(ite.mapInput(transform))
}

class SequenceSourceCache(val cache: SequenceCache) extends SequenceSource {
  def apply(i: Int): Try[Char] = Try(cache.valueAt(i))
  override def reify = Try(this)
  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = cache.enumerate
}

class SequenceSourceReverseCache(val cache: SequenceCache) extends SequenceSource {
  def apply(i: Int): Try[Char] = Try(cache.valueAt(i))
  override def reify = Try(this)
  override def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = cache.enumerateReverse
}

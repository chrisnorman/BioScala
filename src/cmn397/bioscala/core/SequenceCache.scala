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

trait SequenceCache extends Enumerator[Char] {

  val length: Int

  def append(c: Char): SequenceCache
  def valueAt(index: Int): Char

  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = enumerateStep(0)
  def enumerateReverse[R]: Iteratee[Char, R] => Iteratee[Char, R] = reverseEnumerateStep(0)

  protected final def enumerateStep[R](count: Int) : Iteratee[Char, R] => Iteratee[Char, R] = {
    _ match {
      case c @ Continue(f) =>
        if (count != length) enumerateStep(count + 1)(f(Element(valueAt(count))))
        else enumerateStep(count + 1)(f(EndOfInput))
      case other @_ => other
    }
  }

  protected final def reverseEnumerateStep[R](count: Int) : Iteratee[Char, R] => Iteratee[Char, R] = {
    _ match {
      case c @ Continue(f) =>
        if ((length - count) != 0) reverseEnumerateStep(count + 1)(f(Element(valueAt(length - count - 1))))
        else reverseEnumerateStep(count + 1)(f(EndOfInput))
      case other @_ => other
    }
  }
}

object SequenceCache {
  /**
   * Return an iteratee which generates a populated, *unpacked* SequenceCache, suitable for acting as
   * a backing store for a SequenceSourceCache.
   */
  def unpackedCacheGenerator: Iteratee[Char, Try[SequenceCache]] = {
    Iteratee.fold[Char, Try[SequenceCache]](Try(new SequenceCacheUnpacked))((r, e) => r.map(c => c.append(e)))
  }

  /**
   * Return an iteratee which generates a populated, *packed* SequenceCache, suitable for acting as
   * a backing store for a SequenceSourceCache.
   */
  def packedCacheGenerator: Iteratee[Char, Try[SequenceCache]] = {
    Iteratee.fold[Char, Try[SequenceCache]](Try(new SequenceCachePacked))((r, e) => r.map(c => c.append(e)))
  }
}

class SequenceCacheUnpacked private[core](vCache: Vector[Char], val length: Int) extends SequenceCache {

  def this() = this(Vector[Char](), 0)

  def append(c: Char): SequenceCache = new SequenceCacheUnpacked(vCache :+ c, length + 1)
  def valueAt(i: Int): Char = vCache(i)
}

// NOTE: the packed encoding does NOT preserve case in the sequence characters, and always yields
// lower case chars
// NOTE: the length of the vector is limited by the range of Int. This could be extended to handle
// sequences longer than 2MB by using long indices for the cache and mapping them to Int indices
// for the vector since the vector is packed and only takes 1/4n entries to store n values.
class SequenceCachePacked private[core](vCache: Vector[Int], val length: Int) extends SequenceCache {

  def this() 		= this(Vector[Int](), 0)

  private val S 	= 2							// # of bits per char
  private val N 	= 32 / S					// chars per int
  private val M		= (1 << S) -1

  private def globalIndex(i: Int)	= i / N
  private def localIndex(i: Int)	= i % N
  
  private val upperToInt 		= Map[Char, Int]('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
  private val upperFromInt		= Map[Int, Char](0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')
  private def toInt(ch: Char) 	= upperToInt(ch.toUpper)
  private def fromInt(v: Int) 	= upperFromInt(v)

  private def updateAt(i: Int, ch: Char) = vCache(globalIndex(i)) | toInt(ch) << (localIndex(i) * S)
  def valueAt(i: Int): Char = fromInt(vCache(globalIndex(i)) >> (localIndex(i) * S) & M)

  def append(c: Char): SequenceCachePacked = {
    if (length == Int.MaxValue) throw new IllegalArgumentException
    if (length % N == 0)  // vector cache is full and needs to be expanded
      new SequenceCachePacked(vCache :+ (toInt(c) << (localIndex(length) * S)), length + 1)
    else // update vector cache with new value
      new SequenceCachePacked(vCache.updated(globalIndex(length), updateAt(length, c)), length + 1)
  }
}

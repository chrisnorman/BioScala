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

/**
 * In-memory store for strings of sequence characters.
 */
trait SequenceCache
  extends Enumerator[Char]
  //with Traversable[Char]
{

  val length: Int

  def append(c: Char): SequenceCache
  def apply(index: Int): Try[Char]
  def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = enumerateStep(0, it)
  def enumerateReverse[R](it: Iteratee[Char, R]): Iteratee[Char, R] = reverseEnumerateStep(0, it)

  @tailrec
  protected final def enumerateStep[R](count: Int, it: Iteratee[Char, R]): Iteratee[Char, R] = {
    it match {
      case c @ Continue(f) =>
        if (count != length) enumerateStep(count + 1, f(Element(apply(count).get)))
        else enumerateStep(count + 1, f(EndOfInput))
      case other @_ => other
    }
  }

  @tailrec
  protected final def reverseEnumerateStep[R](count: Int, it: Iteratee[Char, R]): Iteratee[Char, R] = {
    it match {
      case c @ Continue(f) =>
        if ((length - count) != 0) reverseEnumerateStep(count + 1, f(Element(apply(length - count - 1).get)))
        else reverseEnumerateStep(count + 1, f(EndOfInput))
      case other @_ => other
    }
  }
}

/**
 * Sequence cache iteratee generators.
 */
object SequenceCache {
  /**
   * Iteratee which generates a populated, *unpacked* SequenceCache, suitable for acting as
   * a backing store for a SequenceSourceCache.
   */
  def unpackedCacheGenerator: Iteratee[Char, Try[SequenceCache]] = {
    Iteratee.fold[Char, Try[SequenceCache]](Try(new SequenceCacheUnpacked))((r, e) => r.map(c => c.append(e)))
  }

  /**
   * Iteratee which generates a populated, *packed* SequenceCache, suitable for acting as
   * a backing store for a SequenceSourceCache.
   */
  def packedCacheGenerator: Iteratee[Char, Try[SequenceCache]] = {
    Iteratee.fold[Char, Try[SequenceCache]](Try(new SequenceCachePacked))((r, e) => r.map(c => c.append(e)))
  }
}

/**
 * An unpacked in-memory store for holding strings of sequence characters.
 */
class SequenceCacheUnpacked private[core](vCache: Vector[Char], val length: Int) extends SequenceCache {

  def this() = this(Vector[Char](), 0)

  def append(c: Char): SequenceCache = new SequenceCacheUnpacked(vCache :+ c, length + 1)
  def apply(i: Int): Try[Char] = Try(vCache(i))
}

/**
 * A packed in-memory store for holding strings of sequnce characters in packed bit
 * representation.
 * 
 * NOTE: the packed encoding does NOT preserve case in the sequence characters, and always
 * yields upper case chars.
 * 
 * NOTE: the length of the vector is limited by the range of Int. (This could be extended
 * to handle longer sequences by using long indices for the sequence itself, and mapping
 * them here to Int indices for the vector, since the vector is packed and only takes 1/4n
 * entries to store n values.
 * 
 */
class SequenceCachePacked private[core](vCache: Vector[Int], val length: Int) extends SequenceCache {

  def this() = this(Vector[Int](), 0)

  // TODO: use the alphabet to determine bits/char rather than hardcoding to 2
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
  def apply(i: Int): Try[Char] = Try(fromInt(vCache(globalIndex(i)) >> (localIndex(i) * S) & M))

  def append(c: Char): SequenceCachePacked = {
    if (length == Int.MaxValue) throw new IllegalArgumentException
    if (length % N == 0)  // vector cache is full and needs to be expanded
      new SequenceCachePacked(vCache :+ (toInt(c) << (localIndex(length) * S)), length + 1)
    else // update vector cache with new value
      new SequenceCachePacked(vCache.updated(globalIndex(length), updateAt(length, c)), length + 1)
  }
}

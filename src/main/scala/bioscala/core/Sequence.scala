/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.core

import scala.util.{ Try, Success, Failure }
import scala.annotation.tailrec

import bioscala.gentypes._

/**
 * Base class for all bio sequences.
 */
// NOTE: the length of these sequences is limited by the range of an Int (Int.MaxValue).
abstract class Sequence(val id: String, val src: SequenceSource)
  extends Traversable[Char]
  with Enumerator[Char]
{
  def apply(i: Int): Try[Char] = src(i)
  def foreach[U](f: Char => U) = src.foreach(f)
  def reify: Try[Sequence] // read this sequence into memory for random access if it isn't already cached
  def reverse: Try[Sequence]
  def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = src.enumerate(it)
  def iterator: Iterator[Char] = src.iterator
  def asString(nChars: Option[Long] = None) = src.asString(nChars)

  /**
   * Returns the Hamming distance between this sequence and the target sequence.
   * 
   * Ideally, if we had enumerator.zip, we could use this Iteratee:
   * 
   *   val hammIt = Iteratee.fold[(Option[Char], Option[Char]), Int](0)((r, e) => {
   *     e match {
   *       case (Some(c1), Some(c2)) => r + (if (c1 == c2) 0 else 1)
   *       case (Some(c), None) => r + 1
   *       case (None, Some(c)) => r + 1
   *       case (None, None) => r
   *      }
   *    })
   *
   *  But until then, use an iterator based solution.
   */
  def getHammingDistance(targetSeq: Sequence): Try[Int] = {
    @tailrec
    def getHamm(it1: Iterator[Char], it2: Iterator[Char], count: Int): Int = {
      if (it1.hasNext) {
        if (it2.hasNext) {
          if (it1.next == it2.next) getHamm(it1, it2, count)
          else getHamm(it1, it2, count+1)
        } else {
          it1.next // discard
          getHamm(it1, it2, count+1)
        }
      } else {
        if (it2.hasNext) {
          it2.next // discard
          getHamm(it1, it2, count+1)
        } else
          count
      }
    }
    Try(getHamm(this.iterator, targetSeq.iterator, 0))
  }

  // @TODO: Do something better than brute force motif search
  /**
   * Returns a literal motif. Brute force. Needs work.
   */
  /*
  def findLiteralMotif(motifLiteral: String): List[Long] = {
    def getMotif(lc: Vector[Char], ll: List[Long], ind: Long): List[Long] = {
      if (lc.isEmpty) ll else
        getMotif(lc.tail, if (lc.startsWith(motifLiteral)) ind +: ll else ll, ind + 1)
    }
    getMotif(getS.toVector, List(), 0).reverse.map(_ + 1)
  }

*/
  /**
   * String representation of this sequence.
   */
  override def toString = id + ": " + src.asString(Some(20))
}

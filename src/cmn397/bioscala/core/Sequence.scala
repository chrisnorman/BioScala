/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.core

import scala.util.{ Try, Success, Failure }

import cmn397.bioscala.gentypes._

/**
 * Base class for all bio sequences.
 */

// NOTE: the length of these sequences is limited by the range of an Int (Int.MaxValue).
abstract class Sequence(val id: String, val src: SequenceSource) {

  // TODO: need to validate enumerate against alphabet
  val alpha: Alphabet[Char]
  
  def apply(i: Int): Try[Char] = src(i)
  def reify: Try[Sequence] // read this sequence into memory for random access if it isn't already cached
  def reverse: Try[Sequence]
  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = src.enumerate(_)
  def getSequenceString(nChars: Option[Long] = None) = src.getSequenceString(nChars)

  /**
   * Returns the Hamming distance between this sequence and the target sequence.
   */
  def getHammingDistance(targetSeq: Sequence): Int = {
    val hammIt = Iteratee.fold[(Option[Char], Option[Char]), Int](0)((r, e) => {
      e match {
        case (Some(c1), Some(c2)) => r + (if (c1 == c2) 0 else 1)
        case (Some(c), None) => r + 1
        case (None, Some(c)) => r + 1
        case (None, None) => r
      }
    })

    def getHamm(seq1: Sequence, seq2: Sequence): Int = {
      // TODO: implement this
      -1
    }
    // simulate enumeration via iteration
    val dist = for {
      seq1 <- this.reify
      seq2 <- targetSeq.reify
    } yield getHamm(seq1, seq2)
    
    dist.getOrElse(-1)
  }

  // @FIX: Do something better than brute force motif search
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
  override def toString = id + ": " + src.getSequenceString(Some(20))
}

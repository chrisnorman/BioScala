/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.core

import cmn397.bioscala.gentypes._

/**
 * Base class for all bio sequences.
 */

// TODO: need to validate enumerate against alphabet
abstract class Sequence(val id: String, val src: SequenceSource) {
  val alpha: Alphabet[Char]

  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = src.enumerate(_)
  def getSequenceString(nChars: Option[Long] = None) = src.getSequenceString(nChars)

  /**
   * Returns the Hamming distance between this sequence and the target sequence.
   */

  def getHammingDistance(targetSeq: Sequence): Long = {
    val ham = Iteratee.fold[(Option[Char], Option[Char]), Long](0)((r, e) => {
      e match {
        case (Some(c1), Some(c2)) => r + (if (c1 == c2) 0 else 1)
        case (Some(c), None) => r + 1
        case (None, Some(c)) => r + 1
        case (None, None) => r
      }
    })
    val zipped = src.zip(targetSeq.src)
    val hDist = zipped.enumerate(ham).result
    hDist.getOrElse(-1)
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

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
abstract class Sequence(id: String, src: SequenceSource) {
  val alpha: Alphabet[Char]

  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = src.enumerate(_)
  def getSequenceString(nChars: Option[Long] = None) = src.getSequenceString(nChars)

  /**
   * Returns the Hamming distance between this sequence and the target sequence.
   */
  /*
  def getHammingDistance(targetSeq: Sequence): Long = {
    def hammingDistance(seq1: Vector[Char], seq2: Vector[Char], acc: Long): Long = {
      if (!seq1.isEmpty) hammingDistance(seq1.tail, seq2.tail, acc + (if (seq1.head != seq2.head) 1 else 0))
      else acc
    }
    hammingDistance(this.getS.toVector, targetSeq.getS.toVector, 0)
  }

  def getHammingDistance(targetSeq: Sequence): Long = {
    def getHamming: Iteratee[Char, Long] = {
      def step(r: Long): Input[Char] => Iteratee[Char, Long] = {
        case Element(e) => 
        case Pending => Done(r, Pending) // ?????????????
        case EndOfInput => Done(r, EndOfInput)
      }
      Continue(step(0))
    }
    src.enumerate(getHamming).result
  }
*/
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

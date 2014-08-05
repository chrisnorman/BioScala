/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.core

import scala.util.{ Try, Success, Failure }

/**
 * 
 */
object ProteinSequence {

  def apply(id: String, seq: String) = new ProteinSequence(id, new SequenceSourceString(seq))
  def apply(fName: String) = new ProteinSequence(fName, new SequenceSourceFASTA(fName))
  def apply(id: String, src: SequenceSource) = new ProteinSequence(id, src)

}

/**
 * A sequence of amino acids representing a polypeptide.
 */
class ProteinSequence(override val id: String, override val src: SequenceSource) extends Sequence(id, src)
{
  val alpha = ProteinAlphabet

  def reify: Try[ProteinSequence] = src.reify.map(s => ProteinSequence(id, s))
  def reverse: Try[ProteinSequence] = src.reify.map(s => new ProteinSequence("Reverse of: " + this.id, s))

  /** Returns the number of possible RNA strings for this protein modulo modN. */
  /*
  def numSourceRNAStrings(modN: Long) : Long = {
    val n = getS.foldLeft(1L)((acc, c) => (RNACodonTable.getNumCandidateCodons(c) * acc) % modN)
    n * 3 % modN
  }
  */
  /** Returns the total mass of this protein in daltons. */
  /*
  def totalMass : Double = getS.foldLeft(0.0)((acc, c) => AminoAcidTable(c).weight + acc)
  */
}


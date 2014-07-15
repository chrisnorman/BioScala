/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package cmn397.bioscala.core

/**
 * A sequence of amino acids representing a polypeptide.
 */
abstract case class ProteinSequence(id: String, val src: SequenceSource) extends Sequence(id, src)
{
  val alpha = ProteinAlphabet

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
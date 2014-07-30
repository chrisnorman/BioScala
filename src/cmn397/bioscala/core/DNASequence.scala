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
import cmn397.bioscala.filehandlers._

/**
 * A sequence of DNA nucleotides.
 */
object DNASequence {

  def apply(id: String, seq: String) = new DNASequence(id, new SequenceSourceString(seq))
  // TODO: PARSING - FASTA file sequence uses the filename for the id rather than the tag inside the file
  def apply(fName: String) = new DNASequence(fName, new SequenceSourceFASTA(fName))
  def apply(id: String, src: SequenceSource) = new DNASequence(id, src)
}

/**
 * 
 */
class DNASequence(override val id: String, override val src: SequenceSource) extends NucleotideSequence(id, src) {
  val alpha = DNAAlphabet

  override def reify: Try[DNASequence] = src.reify.map(s => DNASequence(id, s))
  override def reverse: Try[DNASequence] = src.reverse.map(s => new DNASequence("Reverse of: " + this.id, s))

  /** Returns the RNASequence representing by this DNASequence. */
  def transcribe: Try[RNASequence] = {
    val m = Map[Char, Char]('T' -> 'U', 't' -> 'u')
    Try(new RNASequence("DNA: " + id + ", transcribed",
      new SequenceSourceMappedLinear(src, c => m.getOrElse(c, c))))
  }

  /** Returns the DNASequence representing the reverse complement of this DNASequence. */
  def reverseComplement: Try[DNASequence] = {
    val complementMap = Map[Char, Char](
      'A' -> 'T', 'a' -> 't', 'T' -> 'A', 't' -> 'a', 'C' -> 'G', 'c' -> 'g', 'G' -> 'C', 'g' -> 'c')
    src.reverse.map(s => DNASequence("Reverse complement of: " + id, new SequenceSourceMappedLinear(s, complementMap)))
  }

  override final def countBases: Try[(Long, Long, Long, Long)] = {
    val counter = Iteratee.fold[Char, (Long, Long, Long, Long)](0, 0, 0, 0)((r, e) =>
      e.toLower match {
        case 'a' => (r._1 + 1, r._2, r._3, r._4)
        case 'c' => (r._1, r._2 + 1, r._3, r._4)
        case 'g' => (r._1, r._2, r._3 + 1, r._4)
        case 't' => (r._1, r._2, r._3, r._4 + 1)
      })
    src.enumerate(counter).result
  }

  /**
   * Returns a List of candidate ProteinSequences representing each possible protein to which
   *  this DNA sequence could be translated.
   */
  /*
  def candidateProteins: List[ProteinSequence] = {
    val rnaForwardSeq = transcribe
    val rnaReverseSeq = reverseComplement.transcribe
    rnaForwardSeq.getValidProteins ++ rnaReverseSeq.getValidProteins
  }
*/
}

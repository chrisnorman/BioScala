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
  def apply(id: String, seq: String) = {
    new DNASequence(id, new SequenceSourceString(seq))
  }

  def apply(fName: String) = {
    // TODO: PARSING - FASTA file sequence uses the filename for the id rather than the tag inside the file
    new DNASequence(fName, new SequenceSourceFASTA(fName))
  }

  def apply(id: String, src: SequenceSource) = new DNASequence(id, src)
}

class DNASequence(id: String, src: SequenceSource) extends NucleotideSequence(id, src) {
  val alpha = DNAAlphabet

  /** Returns the ProteinSequence representing by this DNASequence. */
  def transcribe: Try[RNASequence] = {
    val m = Map[Char, Char]('T' -> 'U', 't' -> 'u')
    Success(new RNASequence("DNA: " + id + ", transcribed",
      new SequenceSourceMappedLinear(src, c => m.getOrElse(c, c))))
  }

  /** Returns the DNASequence representing the reverse complement of this DNASequence. */
  def reverseComplement: Try[DNASequence] = {
    val complementMap = Map[Char, Char](
    		'A' -> 'T', 'a' -> 't', 'T' -> 'A', 't' -> 'a', 'C' -> 'G', 'c' -> 'g', 'G' -> 'C', 'g' -> 'c')
    src.enumerate(Iteratee.liftInput[Char, Vector[Char]](SequenceSource.packedVectorGenerator, complementMap)).result match {
      case Success(v) => Success(DNASequence("Reverse complement of " + id, new SequenceSourceReverseVector(v)))
      case Failure(t) => Failure(t)
    }
  }

  override final def countBases: Try[(Long, Long, Long, Long)] = {
    def countNucleotides: Iteratee[Char, (Long, Long, Long, Long)] = {
      def step(r: (Long, Long, Long, Long)): Input[Char] => Iteratee[Char, (Long, Long, Long, Long)] = {
        case Element(e) =>
          e.toLower match {
            case 'a' => Continue(step((r._1 + 1, r._2, r._3, r._4)))
            case 'c' => Continue(step((r._1, r._2 + 1, r._3, r._4)))
            case 'g' => Continue(step((r._1, r._2, r._3 + 1, r._4)))
            case 't' => Continue(step((r._1, r._2, r._3, r._4 + 1)))
            case _ => Error(new IllegalArgumentException)
          }
        case Pending => Done(r, Pending) // ?????????????
        case EndOfInput => Done(r, EndOfInput)
      }
      Continue(step(0, 0, 0, 0))
    }
    src.enumerate(countNucleotides).result
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

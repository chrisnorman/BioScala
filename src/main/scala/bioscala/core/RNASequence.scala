/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.core

import scala.util.{ Try, Success, Failure }

import bioscala.gentypes._

/**
 * A sequence of RNA nucleotides.
 * 
 */
object RNASequence {

  def apply(id: String, seq: String) = { new RNASequence(id, new SequenceSourceString(seq)) }
  def apply(id: String, src: SequenceSource) = { new RNASequence(id, src) }
  def apply(fName: String) = new RNASequence(fName, new SequenceSourceFASTA(fName))

}

/**
 * 
 */
class RNASequence(override val id: String, override val src: SequenceSource) extends NucleotideSequence(id, src) {
  val alpha = RNAAlphabet

  override def reify: Try[RNASequence] = src.reify.map(s => RNASequence(id, s))
  override def reverse: Try[RNASequence] = src.reify.map(s => new RNASequence("Reverse of: " + this.id, s))

  override final def countBases: Try[(Long, Long, Long, Long)] = {
    val counter = Iteratee.fold[Char, (Long, Long, Long, Long)](0, 0, 0, 0)((r, e) =>
      e.toLower match {
        case 'a' => (r._1 + 1, r._2, r._3, r._4)
        case 'c' => (r._1, r._2 + 1, r._3, r._4)
        case 'g' => (r._1, r._2, r._3 + 1, r._4)
        case 'u' => (r._1, r._2, r._3, r._4 + 1)
       }
    )
    src.enumerate(counter).result
  }

  /**
   * Returns true if we can find a valid start codon for this alignment.
   */
  /*
  private def hasValidStartCodon: Boolean = {
    val startORF = getS.grouped(3).toVector.dropWhile(c => (c.length < 3) || (!RNACodonTable.isStartCodon(c.mkString)))
    if (startORF.isEmpty || startORF.head.length < 3) false
    else {
      require(RNACodonTable.isStartCodon(startORF.head.mkString))
      true
    }
  }
*/
  /**
   * Returns true if we can align on a valid start codon followed by a valid stop codon.
   */

 /*   private def hasValidStopCodon : Boolean = {
     enumerate(for {
        a <- Iteratees.take(3)
        b <- if (a.length < 3 || !RNACodonTable.isStartCodon(a.mkString))
     } yield(b))
    val startORF = getS.grouped(3).toVector.dropWhile(c => (c.length < 3) || (!RNACodonTable.isStartCodon(c.mkString)))
    if (startORF.isEmpty || startORF.head.length < 3) false // can't even align on start codon
    else {
      // should be aligned on a start codon
      val stopORF = startORF.dropWhile(c => (c.length == 3) && (!RNACodonTable.isStopCodon(c.mkString)))
      if (stopORF.isEmpty || stopORF.head.length < 3) false
      else {
        require(RNACodonTable.isStopCodon(stopORF.head.mkString))
        true
      }
    }
  }
   private def hasValidStopCodon : Boolean = {
    val startORF = getS.grouped(3).toVector.dropWhile(c => (c.length < 3) || (!RNACodonTable.isStartCodon(c.mkString)))
    if (startORF.isEmpty || startORF.head.length < 3) false // can't even align on start codon
    else {
      // should be aligned on a start codon
      val stopORF = startORF.dropWhile(c => (c.length == 3) && (!RNACodonTable.isStopCodon(c.mkString)))
      if (stopORF.isEmpty || stopORF.head.length < 3) false
      else {
        require(RNACodonTable.isStopCodon(stopORF.head.mkString))
        true
      }
    }
  }
*/
  /**
   * True iff the sequence contains a start codon followed by a stop codon.
   */
  /*
  def isValidORF: Boolean = hasValidStopCodon // test is sufficient since this first aligns on a start codon
*/
  /**
   * Returns index of the locations of all start codons
   */
  /*
  private def getAllStartCodonIndices: List[Long] = {
    def loop(acc: List[Long], seqstr: Vector[Char], curIndex: Long): List[Long] = {
      if (seqstr.isEmpty) acc
      else {
        val nextCodon = seqstr.take(3).mkString
        if (nextCodon.length == 3 && RNACodonTable.isStartCodon(nextCodon))
          loop(curIndex :: acc, seqstr.tail, curIndex + 1)
        else
          loop(acc, seqstr.tail, curIndex + 1)     
      }  
    }
    loop(Nil, getS.toVector, 0).reverse // reverse to keep the lower indices ordered from small to large
  }
*/
  // FIX: get rid of toInts
  /**
   * Return a list of all possible proteins into which this sequence could be translated.
   */
  /*
  def getValidProteins: List[ProteinSequence] = {
    val allSequences = getAllStartCodonIndices.map((i: Long) => RNASequence(id + ", drop:" + i, getS.drop(i.toInt).mkString))
    allSequences.filter((seq: RNASequence) => seq.hasValidStopCodon).map(seq => seq.translate)
   }
*/
  // TODO: This doesn't properly test if there was a stop codon??
  /*
   * Locate the first start codon and translate until an end codon is hit. Assumes the sequence is aligned at
   * and the beginning of the ORF (it need not begin at the first codon, but there should be a start codon).
   */
  /*
  def translate: ProteinSequence = {
    val startORF = getS.grouped(3).toVector.dropWhile(c => (c.length < 3) || (!RNACodonTable.isStartCodon(c.mkString)))
    if (startORF.isEmpty || startORF.head.length < 3)
      throw new Exception("Can't align on start codon")
    else { // should be aligned on a start codon
      val psVec = startORF.takeWhile(c => (c.length == 3) && !RNACodonTable.isStopCodon(c.mkString))
      if (psVec.isEmpty || psVec.head.length < 3)
        throw new Exception("Sequence contains a valid start codon but no valid stop codon")
      else
        ProteinSequence(id + ", as protein", psVec.map(c => RNACodonTable.getAminoAcidShortName(c.mkString)).mkString)
    }
  }
  */
}

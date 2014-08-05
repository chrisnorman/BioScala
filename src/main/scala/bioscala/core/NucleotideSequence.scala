/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package cmn397.bioscala.core

import scala.util.{ Try, Success, Failure }

/**
 * Base class for all sequences of nucleotides (DNASequence and RNASequence).
 */
abstract class NucleotideSequence private[core](override val id: String, override val src: SequenceSource)
	extends Sequence(id, src) {

  /** Returns a count of the individual bases (A,C,G,T or U) for this sequence. */
  def countBases: Try[(Long, Long, Long, Long)]

  /** Returns the GC content (% of all bases which are G & C). */
  def getGCContent: Try[Float] = {
    countBases.map((t : (Long, Long, Long, Long)) => (t._2 + t._3).toFloat / (t._1 + t._2 + t._3 + t._4).toFloat * 100.0.toFloat)
  }
}
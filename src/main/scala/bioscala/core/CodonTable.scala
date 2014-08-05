/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.core

/**
 * Base trait for codon tables.
 */
trait CodonTable {

  protected val codonMap: Map[String, Char] // Map[String, AminoAcid Abbreviation]

  /** Returns true if the string represents a start codon. */
  def isStartCodon(c: String) = codonMap(c) == 'M'

  /** Returns true if the string represents a stop codon. */
  def isStopCodon(c: String) = codonMap(c) == '?'

  /** Returns the if the string represents a stop codon. */
  def getAminoAcidShortName(c: String) = codonMap(c)

  /** Returns true if the string represents a stop codon. */
  def getNumCandidateCodons(c: Char) : Long
}



/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package cmn397.bioscala.core

/**
 * Types for Polar/NonPolar and Acidic/Basic amino acids.
 */
sealed case class AAType

/** Amino Acid type for a polar amino acid. */
final object aaPolar extends AAType

/** Amino Acid type for a nonpolar amino acid. */
final object aaNonPolar extends AAType

/** Amino Acid type for an acidic amino acid. */
final object aaAcidic extends AAType

/** Amino Acid type for a basic amino acid. */
final object aaBasic extends AAType

/** Amino Acid type for a neutral amino acid. */
final object aaNone extends AAType

/**
 * Amino Acid, consisting of a short name, long name, type (polar/non or acidic/basic, and weight in daltons).
 */
case class AminoAcid(val shortName: String, val longName: String, val aaType: AAType, val weight: Double /* daltons*/)

/**
 * 20 common Amino Acids.
 */
object AminoAcidTable {
  /** return the AminoAcid represented by the character. */
  def apply(c: Char) : AminoAcid = aaMap(c)

  private lazy val aaMap = Map(
	    'A' -> AminoAcid("Ala", "Alanine", aaNonPolar, 71.03711),
	    'C' -> AminoAcid("Cys", "Cysteine", aaPolar, 103.00919),
	    'D' -> AminoAcid("Asp", "Aspartic acid", aaAcidic, 115.02694),
	    'E' -> AminoAcid("Glu", "Glutamic acid", aaAcidic, 129.04259),
	    'F' -> AminoAcid("Phe", "Phenylalanine", aaNonPolar, 147.06841),
	    'G' -> AminoAcid("Gly", "Glycine", aaNonPolar, 57.02146),
	    'H' -> AminoAcid("His", "Histidine", aaBasic, 137.05891),
	    'I' -> AminoAcid("Ile", "Isoleucine", aaNonPolar, 113.08406),
	    'K' -> AminoAcid("Lys", "Lysine", aaBasic, 128.09496),
	    'L' -> AminoAcid("Leu", "Leucine", aaNonPolar, 113.08406),
	    'M' -> AminoAcid("Met", "Methionine", aaNonPolar, 131.04049),
	    'N' -> AminoAcid("Asn", "Asparagine", aaPolar, 114.04293),
	    'P' -> AminoAcid("Pro", "Proline", aaNonPolar, 97.05276),
	    'Q' -> AminoAcid("Gln", "Glutamine", aaPolar, 128.05858),
	    'R' -> AminoAcid("Arg", "Arginine", aaBasic, 156.10111),
	    'S' -> AminoAcid("Ser", "Serine", aaPolar, 87.03203),
	    'T' -> AminoAcid("Thr", "Threonine", aaPolar, 101.04768),
	    'V' -> AminoAcid("Val", "Valine", aaNonPolar, 99.06841),
	    'W' -> AminoAcid("Trp", "Tryptophan", aaNonPolar, 186.07931),
	    'Y' -> AminoAcid("Tyr", "Tyrosine", aaPolar, 163.06333)
  )
  
}
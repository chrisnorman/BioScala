/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.core

/**
 * Table of codons for DNA sequence translation.
 */
object DNACodonTable extends CodonTable {

  /** Return the number of codons that map to the amino acid represented by 'aa'. */
  def getNumCandidateCodons(aa: Char) : Long = {
    codonMap.filterKeys(s => codonMap(s) == aa).keys.toList.length
  }

  override lazy protected val codonMap = Map(
    "TTT" -> 'F',
    "TTC" -> 'F',

    "TTA" -> 'L',
    "TTG" -> 'L',

    "CTT" -> 'L',
    "CTC" -> 'L',
    "CTA" -> 'L',
    "CTG" -> 'L',

    "ATT" -> 'I',
    "ATC" -> 'I',
    "ATA" -> 'I',

    "ATG" -> 'M',

    "GTT" -> 'V',
    "GTC" -> 'V',
    "GTA" -> 'V',
    "GTG" -> 'V',

    "TCT" -> 'S',
    "TCC" -> 'S',
    "TCA" -> 'S',
    "TCG" -> 'S',

    "CCT" -> 'P',
    "CCC" -> 'P',
    "CCA" -> 'P',
    "CCG" -> 'P',

    "ACT" -> 'T',
    "ACC" -> 'T',
    "ACA" -> 'T',
    "ACG" -> 'T',

    "GCT" -> 'A',
    "GCC" -> 'A',
    "GCA" -> 'A',
    "GCG" -> 'A',

    "TAT" -> 'Y',
    "TAC" -> 'Y',

    "TAA" -> '?',
    "TAG" -> '?',

    "CAT" -> 'H',
    "CAC" -> 'H',

    "CAA" -> 'Q',
    "CAG" -> 'Q',

    "AAT" -> 'N',
    "AAC" -> 'N',

    "AAA" -> 'K',
    "AAG" -> 'K',

    "GAT" -> 'D',
    "GAC" -> 'D',

    "GAA" -> 'E',
    "GAG" -> 'E',

    "TGT" -> 'C',
    "TGC" -> 'C',

    "TGA" -> '?',
    "TGA" -> '?',

    "TGG" -> 'W',

    "CGT" -> 'R',
    "CGC" -> 'R',
    "CGA" -> 'R',
    "CGG" -> 'R',

    "AGT" -> 'S',
    "AGC" -> 'S',

    "AGA" -> 'R',
    "AGG" -> 'R',

    "GGT" -> 'G',
    "GGC" -> 'G',
    "GGA" -> 'G',
    "GGG" -> 'G')
}

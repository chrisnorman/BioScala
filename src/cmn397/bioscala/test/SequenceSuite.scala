/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package cmn397.bioscala.test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import cmn397.bioscala.core._

@RunWith(classOf[JUnitRunner])class SequenceSuite extends FunSuite {

  // TestID: dna
  test("Count Bases") {
    val seq = DNASequence("TestID: dna", "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
    val res = seq.countBases
    assert(res.isSuccess)
    val (a, c, g, t) = res.get
    assert((a, c, g, t) == (20, 12, 17, 21))
  }

  // TestID: rna
  test("DNA Transcribe") {
    val rnaSeq = DNASequence("TestID: rna", "GATGGAACTTGACTACGTAAATT").transcribe
    assert(rnaSeq.isSuccess && rnaSeq.get.getSequenceString() == "GAUGGAACUUGACUACGUAAAUU")
  }

  // TestID: revc
  test("DNA Reverse Complement") {
    val revComp = DNASequence("TestID: revc", "AAAACCCGGT").reverseComplement
    assert(revComp.isSuccess && revComp.get.getSequenceString() == "ACCGGGTTTT")
  }

  // TestID: gc
  test("GC Content") {
    val seq = DNASequence("TestID: gc", "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG")
    val content = seq.getGCContent
    assert(content.isSuccess)
    assert(content.get - 53.75 < .001)
  }

/*
  // TestID: hamm
  test("Hamming Distance") {
    val seq1 = DNASequence("TestID: hamm1", "GAGCCTACTAACGGGAT")
    val seq2 = DNASequence("TestID: hamm2", "CATCGTAATGACGGCCT")
    assert(seq1.getHammingDistance(seq2) == 7)
  }

  // TestID: prot
  test("Translate to Protein") {
    val seq1 = RNASequence("TestID: prot", "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA")
    val prt = seq1.translate.getS.toVector
    assert(prt == "MAMAPRTEINSTRING".toVector)
  }

  // TestID: subs
  test("Find Literal Motif") {
    val dnaSeq = DNASequence("TestID: subs", "GATATATGCATATACTT")
    val f = dnaSeq.findLiteralMotif("ATAT")
    assert(dnaSeq.findLiteralMotif("ATAT") == List(2, 4, 10))
  }

  // TestID: mprt
  test("Find number of possible source RNAs") {
	  val protSeq = ProteinSequence("test", "MA")
	  val n = protSeq.numSourceRNAStrings(1000000)
	  assert (n == 12)
  }
  
  // TestID: prtm
  test("Total mass of a protein") {
	  val protSeq = ProteinSequence("test", "SKADYEK")
      val n = protSeq.totalMass
      assert( (n - 821.392) < .001)
  }
  *
  */
}


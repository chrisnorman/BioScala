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

import scala.util.{ Try, Success, Failure }

import cmn397.bioscala.core._

@RunWith(classOf[JUnitRunner])class SequenceSuite extends FunSuite {

  test("Sequence") {
    val seq = DNASequence("TestID", "AAAACCCGGT")
    assert(seq.getSequenceString(None) == "AAAACCCGGT")
  }

  test("Sequence: Count DNA Bases") {
    val seq = DNASequence("TestID: dna", "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
    val res = seq.countBases
    assert(res.isSuccess)
    val (a, c, g, t) = res.get
    assert((a, c, g, t) == (20, 12, 17, 21))
  }

  test("Sequence: GC Content") {
    val seq = DNASequence("TestID: gc", "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG")
    val content = seq.getGCContent
    assert(content.isSuccess)
    assert(content.get - 53.75 < .001)
  }

  test("Packed Cache with Reversed Source") {
    val seq = DNASequence("TestID: revc", "AAAACCCGGT")
    // force the sequence into a packed cache, then reverse it
    val packIt = SequenceCache.packedCacheGenerator
    val revSeq = for {
      tryCache <- seq.enumerate(packIt).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield DNASequence("Reverse of " + seq.id, src)

    assert(revSeq.isSuccess && revSeq.get.getSequenceString(None) == "TGGCCCAAAA")
  }

  test("Packed, Mapped Cache with Reversed Source") {
    // force the sequence into a packed cache, map it, then reverse it
    val seq = DNASequence("TestID: revc", "AAAACCCGGT")
    val complementMap = Map[Char, Char](
      'A' -> 'T', 'a' -> 't', 'T' -> 'A', 't' -> 'a', 'C' -> 'G', 'c' -> 'g', 'G' -> 'C', 'g' -> 'c')
    val packIt = SequenceCache.packedCacheGenerator.mapInput(c => complementMap(c))
    val mappedRevSeq = for {
      tryCache <- seq.enumerate(packIt).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield DNASequence("Mapped " + seq.id, src)

    assert(mappedRevSeq.isSuccess && mappedRevSeq.get.getSequenceString(None) == "ACCGGGTTTT")
  }

  test("Packed Cache with Reversed, Mapped Source") {
    // force the sequence into a packed cache, reverse it
    val seq = DNASequence("TestID: revc", "AAAACCCGGT")
    val packIt = SequenceCache.packedCacheGenerator
    val revSeqTry = for {
      tryCache <- seq.enumerate(packIt).result
      cache <- tryCache
      src = new SequenceSourceReverseCache(cache)
    } yield DNASequence("Reversed: " + seq.id, src)
    assert(revSeqTry.isSuccess && revSeqTry.get.getSequenceString(None) == "TGGCCCAAAA")

    // now map it to it's complement
    val revSeq = revSeqTry.get
    val complementMap = Map[Char, Char](
      'A' -> 'T', 'a' -> 't', 'T' -> 'A', 't' -> 'a', 'C' -> 'G', 'c' -> 'g', 'G' -> 'C', 'g' -> 'c')
    val mappedSeq = DNASequence("Mapped: " + revSeq.id, new SequenceSourceMappedLinear(revSeq.src, c => complementMap(c)))
    assert(mappedSeq.getSequenceString(None) == "ACCGGGTTTT")
  }

  /*
  // TestID: hamm
  test("Hamming Distance") {
    val seq1 = DNASequence("TestID: hamm1", "GAGCCTACTAACGGGAT")
    val seq2 = DNASequence("TestID: hamm2", "CATCGTAATGACGGCCT")
    assert(seq1.getHammingDistance(seq2) == 7)
  }
  */

  // TestID: revc
  test("Sequence: Reverse Complement") {
    val revComp = DNASequence("TestID: revc", "AAAACCCGGT").reverseComplement
    assert(revComp.isSuccess)
    val s = revComp.get.getSequenceString()
    assert(s == "ACCGGGTTTT")
  }

  test("Sequence: Reverse Complement of Reverse Complement") {
    val seq = DNASequence("TestID: revc", "AAAACCCGGT")
    val revRevSeq = seq.reverseComplement.flatMap(s => s.reverseComplement)
    assert(revRevSeq.isSuccess)
    val s = revRevSeq.get.getSequenceString(None)
    assert(revRevSeq.get.getSequenceString(None) == seq.getSequenceString(None))
  }

   // TestID: rna
  test("Sequence: DNA Transcription") {
    val rnaSeq = DNASequence("TestID: rna", "GATGGAACTTGACTACGTAAATT").transcribe
    val s = rnaSeq.get.getSequenceString(None)
    assert(rnaSeq.isSuccess && rnaSeq.get.getSequenceString(None) == "GAUGGAACUUGACUACGUAAAUU")
  }

/*
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
  */
}


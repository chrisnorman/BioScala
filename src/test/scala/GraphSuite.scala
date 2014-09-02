/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.util.{ Try, Success, Failure }

import bioscala.core._
import bioscala.filehandlers._
import bioscala.graph._

/**
 * Test the bioscala.graph package (DeBruijn graph, FSM, SuffixTree).
 */
@RunWith(classOf[JUnitRunner]) class GraphSuite extends FunSuite {

  // location of test data files
  val getTestFileDir = "src\\test\\resources\\"

  test("DeBruijn Overlap Pairs from FASTA File") {
    val dbr = DeBruijn.fromFASTAFile(getTestFileDir + "tgrph.FASTA", 5)
    assert(dbr.isSuccess)
    val overlaps = dbr.get.findOverlapPairs
    assert(overlaps == 
      	List(("test_3486", "test_1921"),
      	     ("test_1921", "test_0598"),
      	     ("test_5788", "test_1612"),
      	     ("test_7618", "test_1012"),
      	     ("test_8933", "test_1652"),
      	     ("test_5354", "test_9618"),
      	     ("test_5354", "test_1487"),
      	     ("test_9011", "test_7242")
        )
      )
  }

  test("DeBruijn Overlap Pairs from Sequence List") {
    val ffr = new FASTAFileReader(getTestFileDir + "tgrph.FASTA")
    val res = ffr.reifySequencesPacked.result
    val l = res.map(l => l.map(a => DNASequence(a._1, new SequenceSourceCache(a._2))))
    assert(l.isSuccess)
    val dbr = DeBruijn.fromSequenceList(l.get, 5)
    val overlaps = dbr.findOverlapPairs
    assert(overlaps == 
      	List(("test_3486", "test_1921"),
      	     ("test_1921", "test_0598"),
      	     ("test_5788", "test_1612"),
      	     ("test_7618", "test_1012"),
      	     ("test_8933", "test_1652"),
      	     ("test_5354", "test_9618"),
      	     ("test_5354", "test_1487"),
      	     ("test_9011", "test_7242")
        )
      )
  }
  
  // Test some properties of the FSM; the  test "Find pattern substring" is more thorough
  test("FSM from Pattern") {
    val fsm = FSM.fromPattern("N{P}[ST]{P}".toVector, ProteinAlphabet)
    assert(!fsm.isAcceptState(0))
    assert(!fsm.isAcceptState(1))
    assert(!fsm.isAcceptState(2))
    assert(!fsm.isAcceptState(3))
    assert(fsm.isAcceptState(4))
    assert(fsm.getValidEvents(2) == Vector('S', 'T'))
  }
  
  test("Suffix Tree from String - Longest Common Substring") {
    val st0 = new SuffixTree
    val st1 = st0.updated("GATTACA$", "id1")
    val st2 = st1.updated("TAGACCA#", "id2")
    val st3 = st2.updated("ATACA&", "id3")
    val lst = st3.lcs
    assert (lst.length === 3)
    assert (lst.contains("AC") && lst.contains("TA") && lst.contains("CA"))
  }

  test("Suffix Tree From Sequences - Longest Common Substring") {
    val st = SuffixTree.fromFASTAFile(getTestFileDir + "tlcs.FASTA")
    assert(st.isSuccess)
    val lst = st.get.lcs
    assert (lst.length === 3)
    assert (lst.contains("AC") && lst.contains("TA") && lst.contains("CA"))
  }

  test("Suffix Tree from String - Find Frequent K-mer") {
    val st0 = new SuffixTree
 	val st1 = st0.updated("ACGTTGCATGTCGCATGATGCATGAGAGCT$", "id1") // CATG(3) and GCAT(3)
    val lst = st1.mostFrequentKmers(4)
    assert(lst.length == 2)
    assert(lst.contains(("CATG", 3)) && lst.contains(("GCAT", 3)))
  }

  test("Suffix Tree from String - Find Pattern Substring") { 	
    val proteinAlphabet = "ACDEFGHIKLMNPQRSTVWY".toVector
	val st = new SuffixTree().updated("ABNDSDEHK#", "test pattern match")
    val l = st.matchPattern(ProteinAlphabet, "N{P}[ST]{P}")
    assert(l == List((2, 4)))
  }
}

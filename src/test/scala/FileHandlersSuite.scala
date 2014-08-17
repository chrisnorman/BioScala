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
import bioscala.gentypes._
import bioscala.filehandlers._

/**
 * 
 */
@RunWith(classOf[JUnitRunner]) class FileHandlersSuite extends FunSuite {

  // location of test data files
  val getTestFileDir = "src\\test\\resources\\"

  // TestID: 
  test("FASTA File Source - single sequence") {
    val seq = DNASequence(getTestFileDir + "thammdist1.FASTA")
    val s = seq.getSequenceString(None)
    assert(s == "TAACAAGTGTGAGG")
  }

  // TestID: 
  test("FASTA File Source - multi sequence") {
    val seq = DNASequence(getTestFileDir + "tcons.FASTA")
    val s = seq.getSequenceString(None)
    assert(s == "ATCCAGCT")
  }

  test("FASTA File Source - enumerate (takeRight)") {
    val seq = DNASequence(getTestFileDir + "tcons.fasta")
    val resIt = seq.enumerate(Iteratees.takeRight(4))
    val res = resIt.result.map(_.mkString).getOrElse("failed")
    assert(res == "AGCT")
  }
  
  test("FASTA File Reader - enumerateSequencesPacked") {
    val ffr = new FASTAFileReader(getTestFileDir + "tcons.fasta")
    val res = ffr.reifySequencesPacked.result
    val s = res.map(l => l.map(a => (a._1, DNASequence(a._1, new SequenceSourceCache(a._2)).getSequenceString())))
    assert(s.get.length == 7)

    val v1 = s.get(0)._2.mkString 
    val v2 = s.get(0)._1.mkString

    assert(s.get(0)._2.mkString == "ATCCAGCT")
    assert(s.get(0)._1.mkString == "test_1")
    assert(s.get(1)._2.mkString == "GGGCAACT")
    assert(s.get(1)._1.mkString == "test_2")
    assert(s.get(2)._2.mkString == "ATGGATCT")
    assert(s.get(2)._1.mkString == "test_3")
    assert(s.get(3)._2.mkString == "AAGCAACC")
    assert(s.get(3)._1.mkString == "test_4")
    assert(s.get(4)._2.mkString == "TTGGAACT")
    assert(s.get(4)._1.mkString == "test_5")
    assert(s.get(5)._2.mkString == "ATGCCATT")
    assert(s.get(5)._1.mkString == "test_6")
    assert(s.get(6)._2.mkString == "ATGGCACT")
    assert(s.get(6)._1.mkString == "test_7")
  }

  test("FASTA File Reader - enumerateSequencesUnpacked") {
    val ffr = new FASTAFileReader(getTestFileDir + "tcons.fasta")
    val res = ffr.reifySequencesUnpacked.result
    val s = res.map(l => l.map(a => (a._1, DNASequence(a._1, new SequenceSourceCache(a._2)).getSequenceString())))
    assert(s.get(0)._2.mkString == "ATCCAGCT")
    assert(s.get(0)._1.mkString == "test_1")
    assert(s.get(1)._2.mkString == "GGGCAACT")
    assert(s.get(1)._1.mkString == "test_2")
    assert(s.get(2)._2.mkString == "ATGGATCT")
    assert(s.get(2)._1.mkString == "test_3")
    assert(s.get(3)._2.mkString == "AAGCAACC")
    assert(s.get(3)._1.mkString == "test_4")
    assert(s.get(4)._2.mkString == "TTGGAACT")
    assert(s.get(4)._1.mkString == "test_5")
    assert(s.get(5)._2.mkString == "ATGCCATT")
    assert(s.get(5)._1.mkString == "test_6")
    assert(s.get(6)._2.mkString == "ATGGCACT")
    assert(s.get(6)._1.mkString == "test_7")
  }

}


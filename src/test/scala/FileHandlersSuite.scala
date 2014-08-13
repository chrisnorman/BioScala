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
    val s = res.map(l => l.map(a => DNASequence(a._1, new SequenceSourceCache(a._2)).getSequenceString()))
    assert(s.isSuccess)
    assert(s.get.length == 7)
    assert(s.get(0).mkString == "ATCCAGCT")
    assert(s.get(1).mkString == "GGGCAACT")
    assert(s.get(2).mkString == "ATGGATCT")
    assert(s.get(3).mkString == "AAGCAACC")
    assert(s.get(4).mkString == "TTGGAACT")
    assert(s.get(5).mkString == "ATGCCATT")
    assert(s.get(6).mkString == "ATGGCACT")
  }

  test("FASTA File Reader - enumerateSequencesUnpacked") {
    val ffr = new FASTAFileReader(getTestFileDir + "tcons.fasta")
    val res = ffr.reifySequencesUnpacked.result
    val s = res.map(l => l.map(a => DNASequence(a._1, new SequenceSourceCache(a._2)).getSequenceString()))
    assert(s.isSuccess)
    assert(s.get.length == 7)
    assert(s.get(0).mkString == "ATCCAGCT")
    assert(s.get(1).mkString == "GGGCAACT")
    assert(s.get(2).mkString == "ATGGATCT")
    assert(s.get(3).mkString == "AAGCAACC")
    assert(s.get(4).mkString == "TTGGAACT")
    assert(s.get(5).mkString == "ATGCCATT")
    assert(s.get(6).mkString == "ATGGCACT")
  }

}


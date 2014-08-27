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
import bioscala.graph._

/**
 * 
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

  test("DeBruijn Overlap Pairs from sequence list") {
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
}

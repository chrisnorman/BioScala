
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

/**
 * 
 */
@RunWith(classOf[JUnitRunner]) class ConsensusSuite extends FunSuite {

  // location of test data files
  val getTestFileDir = "src\\test\\resources\\"

  test("Profile and Consensus") {
    val ffr = new FASTAFileReader(getTestFileDir + "tcons.FASTA")
    val res = ffr.reifySequencesPacked.result
    val l = res.map(l => l.map(a => DNASequence(a._1, new SequenceSourceCache(a._2))))
    assert(l.isSuccess)
    val seqList = l.get
    val profileMatrix = List((5,0,1,1), (1,0,1,5), (0,1,6,0), (0,4,3,0), (5,2,0,0), (5,0,1,1), (0,6,0,1), (0,1,0,6))
    val consensusString = "ATGCAACT".toList
    val (prof, cons) = Consensus.getConsensus(seqList)
    assert(profileMatrix == prof)
    assert(consensusString == cons)
  }
}


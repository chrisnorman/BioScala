/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

import scala.io.Source

import bioscala.core._
import bioscala.gentypes._
import bioscala.filehandlers.FASTAFileReader
import bioscala.graph._

/**
 * Test driver application for exercising  BioScala functions.
 */
object BioScalaTestDriver {

  // location of test data files
  val getTestFileDir = "src\\test\\resources\\"
    
  def doBioScala(args: Int): Unit = {
	args match {
      case 1 => // countBases
       			val seq = DNASequence("id", "acgt")
       			println(seq.countBases)

      case 2 => // transcribe
       			val inputFile = getTestFileDir + "trna.txt" // called tRNA, but it contains a DNA string
      			val seq2 = DNASequence("Test transcribe", Source.fromFile(inputFile).getLines.mkString)
      			val tseq = seq2.transcribe
     			println(tseq)

     case 3 => // reverseComplement
       			val inputFile = getTestFileDir + "trevc.txt"
      			val seq2 = DNASequence("test", Source.fromFile(inputFile).getLines.mkString)
     			val s = seq2.reverseComplement.get.asString()
     			println(s)
   
      case 4 => // hamming distance
		        val List(s1, s2) = Source.fromFile(getTestFileDir + "thamm.txt").getLines.toList
		        val seq1 = DNASequence("hamming 1", s1)
		        val seq2 = DNASequence("hamming 2", s2)
      			println(seq1.getHammingDistance(seq2))

      			val seq = DNASequence(getTestFileDir + "thammdist1.FASTA")
       			val targ = DNASequence(getTestFileDir + "thammdist2.FASTA")
       			val dist = targ.getHammingDistance(seq)
       			println("Hamming distance: " + dist)

      case 5 => // string source enumerate
     			val seq = DNASequence("id", "aacccgtaacgtg")
      			val res = seq.enumerate {
     			  for {
     			    a <- Iteratees.take[Char](5)
      			    b <- Iteratees.takeRight[Char](5)
      			  } yield (a, b)
     			}.result
      			println(res.get)

      case 6 => // FASTA file *source* enumerate
     			val seq = DNASequence(getTestFileDir + "tcons.FASTA")
     			val res = seq.enumerate(Iteratees.takeRight(8))
       			println(res.result)

      case 7 => // FASTA file *reader* reifySequences
      			val ffr = new FASTAFileReader(getTestFileDir + "tlcsm.FASTA")
     			val res = ffr.reifySequencesPacked.result
     			res.map(l => l.map(a => println(DNASequence(a._1, new SequenceSourceCache(a._2)).asString())))

      case 8 => // *large* FASTA file reader reifySequences
       			val ffr = new FASTAFileReader("\\Sharing\\Development\\TestFiles\\chr22.FASTA")
       			val seqList = ffr.reifySequencesPacked
       			val seq = DNASequence(seqList.result.get.head._1, new SequenceSourceCache(seqList.result.get.head._2))
       			val gc1 = seq.getGCContent
       			println(gc1)
       			val gc2 = seq.reverseComplement.get.getGCContent
       			println(gc2)
     			
      case 9 =>  // FASTA file reader enumerateResult/DeBruijn graph creation/find overlaps
       			val res = DeBruijn.fromFASTAFile(getTestFileDir + "tgrph.FASTA", 5)
       			if (res.isSuccess)
       			  println("Overlaps: \n" + res.get.findOverlapPairs)

      case other => println(other + ": unrecognized problem number")
    }
  }

  def main(args: Array[String]): Unit = doBioScala(9)
}


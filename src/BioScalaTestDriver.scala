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
//import bioscala.graph._

/**
 * Test driver application for exercising  BioScala functions.
 */
object BioScalaTestDriver {

  // location of test data files
  val getTestFileDir = "src\\test\\resources\\"
    
  def doBioScala(args: Int): Unit = {
	args match {
      case 1 =>
       			val seq = DNASequence("id", "acgt")
       			println(seq.countBases)

      case 2 => // TestID: rna
       			val inputFile = getTestFileDir + "trna.txt" // called tRNA, but it contains a DNA string
      			val seq2 = DNASequence("Test transcribe", Source.fromFile(inputFile).getLines.mkString)
      			val tseq = seq2.transcribe
     			println(tseq)

     case 3 => // TestID: revc
       			val inputFile = getTestFileDir + "trevc.txt"
      			val seq2 = DNASequence("test", Source.fromFile(inputFile).getLines.mkString)
     			val s = seq2.reverseComplement.get.asString()
     			println(s)
   
      case 4 => // TestID: hamm
		        //val List(s1, s2) = Source.fromFile(getTestFileDir + "thamm.txt").getLines.toList
		        //val seq1 = DNASequence("hamming 1", s1)
		        //val seq2 = DNASequence("hamming 2", s2)
      			//println(seq1.getHammingDistance(seq2))

      			val seq = DNASequence(getTestFileDir + "thammdist1.FASTA")
       			val targ = DNASequence(getTestFileDir + "thammdist2.FASTA")
       			val dist = targ.getHammingDistance(seq)
       			println("Hamming distance: " + dist)

      case 5 => // string source enumerate test
     			val seq = DNASequence("id", "aacccgtaacgtg")
      			val res = seq.enumerate {
     			  for {
     			    a <- Iteratees.take[Char](5)
      			    b <- Iteratees.takeRight[Char](5)
      			  } yield (a, b)
     			}.result
      			println(res.get)

      case 6 => // FASTA file source enumerate test
     			val seq = DNASequence(getTestFileDir + "tcons.fasta")
     			val res = seq.enumerate(Iteratees.takeRight(8))
       			println(res.result)

      case 7 => // FASTA file *reader* enumerate test
      			val ffr = new FASTAFileReader(getTestFileDir + "tlcsm.fasta")
     			val res = ffr.reifySequencesPacked.result
     			res.map(l => l.map(a => println(DNASequence(a._1, new SequenceSourceCache(a._2)).asString())))
     			/*val seqs = for {
     			  a <- res.get
     			  s = DNASequence(a._1, new SequenceSourceCache(a._2))
     			} yield(s)
     			seqs.map(s => println(s.id + ": " + s.asString()))*/		  

      case 8 => // giant FASTA file source processing
       			val ffr = new FASTAFileReader("\\Sharing\\Development\\TestFiles\\chr22.FASTA")
       			val seqList = ffr.reifySequencesPacked
       			val seq = DNASequence(seqList.result.get.head._1, new SequenceSourceCache(seqList.result.get.head._2))
       			val gc1 = seq.getGCContent
       			println(gc1)
       			val gc2 = seq.reverseComplement.get.getGCContent
       			println(gc2)
 /*      			
      case 9 =>
       			val ffr = new FASTAFileReader(getTestFileDir + "tcons.fasta")
       			val resT = ffr.enumerateResult(
       			    Iteratee.fold[SequenceCache, List[String]](Nil)(
       			        (r, e) => {
       			          new DNASequence("id", new SequenceSourceCache(e)).asString() +: r
       			        }
       			    )
       			)
       			val res = resT.result
       			if (res.isSuccess)
       			  println(res.get)
*/
/*
      case 6 => // TestID: perm
      			val l = (1 to 5).toList.permutations
      			println(l)

      case 7 => // TestID: prot
        		val inputFile = getTestFileDir + "tprot.txt"
      			val seq = RNASequence("test", Source.fromFile(inputFile).getLines.mkString)
      			println(seq.translate)

      case 8 => // TestID: subs
        		val List(s, p) = Source.fromFile(getTestFileDir + "tsubs.txt").getLines.toList
        		val dnaSeq = DNASequence("id", s)
      			println(dnaSeq.findLiteralMotif(p))

      case 9 => // TestID: cons
		         val ff = new FASTAFileReader(getTestFileDir + "tcons.fasta")
		         val fList = ff.getSequenceList
		         val (profile, consensus) = SequenceAnalysis.getConsensusProfileAndString(fList)
		         SequenceAnalysis.printProfile(profile)
		         println(consensus)

      case 10 => // TestID: grph
		         val ff = new FASTAFileReader(getTestFileDir + "tgrph.fasta")
		         val dbg = DeBruijn.graphFromSequenceList(3, ff.getSequenceList)
		         dbg.findOverlapPairs.foreach((t) => println(t._1 + " " + t._2))

      case 11 => // testID: lcsm
        		 val ff = new FASTAFileReader(getTestFileDir + "tlcsm.fasta")
		         val mList = ff.getSequenceList
		         val g = mList.foldLeft(new SuffixTree)((g: SuffixTree, d: DNASequence) => g.updated(d.getS.mkString + "$", d.id))
		         val lst = mList.foldLeft(List[String]())((s: List[String], d: DNASequence) => d.id.mkString :: s)
		         val result = g.LongestCommonSubstring
		         result.foreach(println(_))

      case 12 => // TestID: mprt
        		FindNGlycosylationMotifLocations
      
      case 13 => // TestID: mrna
        		val inputFile = getTestFileDir + "tmrna.txt"
        	    val protSeq = ProteinSequence("mrna", Source.fromFile(inputFile).getLines.mkString)
        		val n = protSeq.numSourceRNAStrings(1000000)
        		println(n)
        		 
      case 14 => // TestID: prtm
        		val inputFile = getTestFileDir + "tprtm.txt"
        	    val protSeq = ProteinSequence("mrna", Source.fromFile(inputFile).getLines.mkString)
        		val n = protSeq.totalMass
        		println(n)
        		 
      case 15 => // TestID: orf
		        val ff = new FASTAFileReader(getTestFileDir + "torf.fasta")
		        val dnaSeqList = ff.getSequenceList
		        dnaSeqList match {
		          case seq :: Nil => val proteins = dnaSeqList.head.candidateProteins.map(_.getS.mkString).toSet.toList.sortWith(_ < _)
		        		  			 proteins.foreach(println(_))
		          case _ => println("Expected a single sequence")
		        }
		
*/
      case other => println(other + ": unrecognized problem number")
    }
   }

  def main(args: Array[String]): Unit = {
    doBioScala(9)
  }

/*
  // TestID: mprt find n glycosylation motif locations in a series of proties speciied by UniPtot ID
  def FindNGlycosylationMotifLocations = {
    import scala.io.BufferedSource
	import java.io.{InputStreamReader, BufferedReader, ByteArrayInputStream}
	val fis = new java.io.FileInputStream(getTestFileDir + "tmprt.txt")  // contains a list of UniProt protein IDs
	val bs = new BufferedSource(fis)
	val proteinIDList = bs.getLines().toList
	def getUniProtFastaFile(id: String) : FASTAFileReader = {
	  val baseURL = "http://www.uniprot.org/uniprot/" + id + ".fasta"
	  val result = scala.io.Source.fromURL(baseURL).mkString
	  println("Protein: " + id + "Length: " + result.length)
	  new FASTAFileReader(new ByteArrayInputStream(result.getBytes))
	}
	val fastaList = proteinIDList.map(getUniProtFastaFile(_))
	val proteinAlphabet = "ACDEFGHIKLMNPQRSTVWY".toVector
	def getNGlycosylationMotifLocations(ff: FASTAFileReader) : List[(Long, Long)] = {
	  require (ff.getSequenceList.length == 1)
	  val DNASeq = ff.getSequenceList.head
	  val st = new SuffixTree().updated(DNASeq.getS.mkString + "#", DNASeq.id)
	  st.substringsFromPattern(ProteinAlphabet, "N{P}[ST]{P}")
	}
	val resultList = fastaList.map(getNGlycosylationMotifLocations)
	fis.close
	def printList(e: (String, List[(Long, Long)])) = {
	  println(e._1);
	  e._2.sorted.foreach(g => print((g._1 + 1) + " "))
	  println
	}
	proteinIDList.zip(resultList).foreach(printList)
  }
  */
}

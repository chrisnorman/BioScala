/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.filehandlers

import scala.util.{ Try, Success, Failure }
import scala.io.BufferedSource
import scala.annotation.tailrec
import scala.util.control.NonFatal

import bioscala.core._
import bioscala.gentypes._

/**
 * FASTAFileReader: Used to read FASTA files containing multiple sequences. The iteratee
 * supplied to the enumerator is run against each sequence in the file, accumulating
 * a List of results of type R, which may be anything such as a DeBruijn graph, a suffix tree, or a
 * List[Sequence] where each sequence has been reified as a string or cache.
 *
 */
class FASTAFileReader(fileName: String) {

  private case object EndOfUnit extends Input[Nothing] // private sentinel for signaling end of a sequence

  // fake an end-of-input for the iteratee, but return EndOfUnit to
  // the enumerator loop to force the start of a new seed
  private def stepIt[R](charIt: Iterator[Char], f: Input[Char] => Iteratee[Char, R]) : Iteratee[Char, R] = {
    if (charIt.hasNext) {
      val c = charIt.next
	  if (c == '>') {
        f(EndOfInput) match {
          case Done(res, _) => Done(res, EndOfUnit)
          case other @ _ => other
        }
	  }
	  else f(Element(c))
    }
    else f(EndOfInput)
  }

  @tailrec
  private def loop[R](
      charIt: Iterator[Char],
	  currentIt: Iteratee[Char, R],		// iteratee in progress
	  restartIt: Iteratee[Char, R],		// iteratee to use on EndOfUnit
	  acc: List[R]
  ): Try[List[R]] =
  {
    currentIt match {
	  case Continue(f) =>
	    val nextIt = stepIt(charIt, f)
	    nextIt match {
		  case Continue(f) => loop(charIt, nextIt, restartIt, acc)
	      case Done(res, EndOfUnit) => loop(charIt, restartIt, restartIt, res :: acc) // start over with a new seed/iteratee
	      case Done(res, EndOfInput) => Success(res :: acc)
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Success(res :: acc)
	}
  }

  def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, List[(String, R)]] = {
    val tFIS = Try(new java.io.FileInputStream(fileName))
    if (tFIS.isFailure)
      Error(tFIS.failed.get)
    else {
      val tBS = Try(new BufferedSource(tFIS.get))
      if (tBS.isFailure) {
        tFIS.get.close
        Error(tBS.failed.get)
      }
      else {
        val charIt = tBS.get.iter
        val compoundIt = for {
	      a <- Iteratees.takeLine		// eat the FASTA header line
	      b <- it
	    } yield(a, b)

	    val tChev = Try{charIt.hasNext && charIt.next == '>'}
	    if (tChev.isSuccess) {
          val resList = loop(charIt, compoundIt, compoundIt, Nil)
          tBS.get.close
          tFIS.get.close
          if (resList.isSuccess) Done(resList.get.reverse, EndOfInput)
          else Error(resList.failed.get)
	    }
	    else {
          tBS.get.close
          tFIS.get.close
	      Error(new IllegalArgumentException("Can't parse FASTA file header"))
	    }
      }
    }
  }

  def enumerateList[R](seed: R, f: (Char, R) => R) : Iteratee[Char, List[(String, R)]] = {
    def step(r: R): Input[Char] => Iteratee[Char, R] = in => {
      in match {
        case Element(e) =>
     	  if (e == '\n' || e == '\r') Continue(step(r))  // strip out embedded cr/lf
     	  else Continue(step(f(e, r)))
     	case EndOfInput => Done(r, EndOfInput)
       }
    }
    enumerate[R](Continue(step(seed)))
  }

  /*
   * Return a list of all sequences in the FASTA file reified in packed cache (use for
   * DNASequences).
   */
  def reifySequencesPacked: Iteratee[Char, List[(String, SequenceCachePacked)]] = 
		  enumerateList[SequenceCachePacked](new SequenceCachePacked, (e, r) => (r.append(e)))

  /*
   * Return a list of all sequences in the FASTA file reified in packed cache (use for
   * RNASequences or ProteinSequences).
   */
  def reifySequencesUnpacked: Iteratee[Char, List[(String, SequenceCacheUnpacked)]] =
		  enumerateList[SequenceCacheUnpacked](new SequenceCacheUnpacked, (e, r) => (r.append(e)))
}

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

import bioscala.core.{ DNASequence, Sequence, SequenceSourceCache, SequenceCache, SequenceCachePacked }
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
  private def stepIt[R](srcIt: Iterator[Char], f: Input[Char] => Iteratee[Char, R]) : Iteratee[Char, R] = {
    if (srcIt.hasNext) {
      val c = srcIt.next
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
	  currentIt: Iteratee[Char, R],
	  restartIt: Iteratee[Char, R],
	  rList: List[R]
  ): Try[List[R]] =
  {
    currentIt match {
	  case Continue(f) =>
	    val nextIt = stepIt(charIt, f)
	    nextIt match {
		  case Continue(f) => loop(charIt, nextIt, restartIt, rList)
	      case Done(res, EndOfUnit) => loop(charIt, restartIt, restartIt, res :: rList) // start over with a new seed/iteratee
	      case Done(res, EndOfInput) => Success(res :: rList)
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Success(res :: rList)
	}
  }

  def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, List[R]] = {
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
	    } yield(b)

	    val tChev = Try{charIt.hasNext && charIt.next == '>'}
	    if (tChev.isSuccess) {
          val resList = loop(charIt, compoundIt, compoundIt, Nil)
          tBS.get.close
          tFIS.get.close
          if (resList.isSuccess) Done(resList.get.reverse, EndOfInput)
          else Error(resList.failed.get)
	    }
	    else Error(new IllegalArgumentException("Can't parse FASTA file header"))
      }
    }
  }

  def enumerateList[R](seed: R, f: (Char, R) => R) : Iteratee[Char, List[R]] = {
    def step(r: R): Input[Char] => Iteratee[Char, R] = in => {
      in match {
        case Element(e) =>
     	  if (e == '\n' || e == '\r') Continue(step(r))
     	  else Continue(step(f(e, r)))
     	case EndOfInput => Done(r, EndOfInput)
       }
    }
    enumerate[R](Continue(step(seed)))
  }
  
  def enumerateSequencesPacked: Iteratee[Char, List[SequenceCachePacked]] = 
		  enumerateList[SequenceCachePacked](new SequenceCachePacked, (e, r) => r.append(e))
}


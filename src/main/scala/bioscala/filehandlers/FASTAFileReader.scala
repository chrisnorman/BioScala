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
class FASTAFileReader(fileName: String) extends FASTAFileParser {

  @tailrec
  private def loopResultList[R](
    charIt: Iterator[Char],
	currentIt: Iteratee[Char, R],		// iteratee in progress
	restartIt: Iteratee[Char, R],		// iteratee to use on EndOfUnit/restart
	acc: List[R]
  ): Try[List[R]] =
  {
    currentIt match {
	  case Continue(f) =>
	    val nextIt = stepIt(charIt, f, EndOfUnit)
	    nextIt match {
		  case Continue(f) => loopResultList(charIt, nextIt, restartIt, acc)
	      case Done(res, EndOfUnit) => loopResultList(charIt, restartIt, restartIt, res :: acc) // start over with a new seed/iteratee
	      case Done(res, EndOfInput) => Success(res :: acc)
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Success(res :: acc)
	}
  }

  def enumerateResultList[R](it: Iteratee[Char, R]): Iteratee[Char, List[(String, R)]] = {
    val compoundIt = for {
	  a <- Iteratees.takeLine		// FASTA sequence header line
	  b <- liftFilter(it)			// lift the user-supplied iteratee with a cr/lf filter
	} yield (a, b)

    def enumLoop(charIt: Iterator[Char]): Iteratee[Char, List[(String, R)]] = {
      val resList = loopResultList(charIt, compoundIt, compoundIt, Nil)
      if (resList.isSuccess)
        Done(resList.get.reverse, EndOfInput)
      else
        Error(resList.failed.get)
    }

    enumerateT(fileName, enumLoop)
  }

/*
  @tailrec
  private def loopResult[R](
    charIt: Iterator[Char],
	currentIt: Iteratee[Char, (String, Iteratee[SequenceCache, R])]		// iteratee in progress
  ): Try[R] =
  {
    currentIt match {
	  case Continue(f) =>
	    val nextIt = stepIt(charIt, f, EndOfUnit)
	    nextIt match {
		  case Continue(f) => loopResult(charIt, nextIt)
	      case Done(res, EndOfUnit) => loopResult(charIt, nextIt)
	      case Done(res, EndOfInput) => res._2.result
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Success(res._2.result.get)
	}
  }

  def enumerateResult[R](it: Iteratee[SequenceCache, R]): Iteratee[Char, R] = {
    val compoundIt = for {
	  a <- Iteratees.takeLine		// FASTA sequence header line
	  b <- liftFilter(Iteratee.fold[Char, SequenceCache](new SequenceCachePacked)((r, e) => (r.append(e)))).map(c => it)	// lift the user-supplied iteratee with a cr/lf filter
	} yield (a, b)

    def enumLoop(charIt: Iterator[Char]): Iteratee[Char, R] = {
      val resList = loopResult(charIt, compoundIt)
      if (resList.isSuccess) Done(resList.get, EndOfInput)
      else Error(resList.failed.get)
    }

    enumerateT(fileName, enumLoop)
  }
*/
  /*
   * Return a list of all sequences in the FASTA file reified in a packed cache (use for
   * DNASequences).
   */
  def reifySequencesPacked: Iteratee[Char, List[(String, SequenceCachePacked)]] =
    enumerateResultList(Iteratee.fold[Char, SequenceCachePacked](new SequenceCachePacked)((r, e) => (r.append(e))))

  /*
   * Return a list of all sequences in the FASTA file reified in an unpacked cache (use for
   * RNASequences or ProteinSequences).
   */
  def reifySequencesUnpacked: Iteratee[Char, List[(String, SequenceCacheUnpacked)]] =
    enumerateResultList(Iteratee.fold[Char, SequenceCacheUnpacked](new SequenceCacheUnpacked)((r, e) => (r.append(e))))
}

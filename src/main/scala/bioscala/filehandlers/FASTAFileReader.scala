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

  @tailrec
  private def loopResult[R](
    charIt: Iterator[Char],
	currentIt: Iteratee[Char, (String, SequenceCache)],		// iteratee in progress
	restartIt: Iteratee[Char, (String, SequenceCache)],		// iteratee to use on EndOfUnit/restart
	accum: R,												// state accumulator
	mapF: (R, String, SequenceCache) => R					// state transition function
  ): Try[R] =
  {
    currentIt match {
	  case Continue(f) =>
	    val nextIt = stepIt(charIt, f, EndOfUnit)
	    nextIt match {
		  case Continue(f) => loopResult(charIt, nextIt, restartIt, accum, mapF)
	      case Done(res, EndOfUnit) => loopResult(charIt, restartIt, restartIt, mapF(accum, res._1, res._2), mapF)
	      case Done(res, EndOfInput) => Try(mapF(accum, res._1, res._2))
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Try(mapF(accum, res._1, res._2))
	}
  }

  def enumerateResult[R](seed: R, mapF: (R, String, SequenceCache) => R): Iteratee[Char, R] = {
    val compoundIt = for {
	  a <- Iteratees.takeLine		// FASTA sequence header line
	  b <- liftFilter(Iteratee.fold[Char, SequenceCache](new SequenceCachePacked)((r, e) => (r.append(e))))	// lift the user-supplied iteratee with a cr/lf filter
	} yield (a, b)

    def enumLoop(charIt: Iterator[Char]): Iteratee[Char, R] = {
      val resList = loopResult(charIt, compoundIt, compoundIt, seed, mapF)
      if (resList.isSuccess) Done(resList.get, EndOfInput)
      else Error(resList.failed.get)
    }

    enumerateT(fileName, enumLoop)
  }

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

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
  private def loopList[R](
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
		  case Continue(f) => loopList(charIt, nextIt, restartIt, acc)
	      case Done(res, EndOfUnit) => loopList(charIt, restartIt, restartIt, res :: acc) // start over with a new seed/iteratee
	      case Done(res, EndOfInput) => Success(res :: acc)
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Success(res :: acc)
	}
  }

  def enumerateList[R](it: Iteratee[Char, R]): Iteratee[Char, List[(String, R)]] = {
    val compoundIt = for {
	  a <- Iteratees.takeLine		// FASTA sequence header line
	  b <- liftFilter(it)			// lift the user-supplied iteratee with a cr/lf filter
	} yield (a, b)

    def enumLoop(charIt: Iterator[Char]): Iteratee[Char, List[(String, R)]] = {
      val resList = loopList(charIt, compoundIt, compoundIt, Nil)
      if (resList.isSuccess)
        Done(resList.get.reverse, EndOfInput)
      else
        Error(resList.failed.get)
    }

    enumerateT(fileName, enumLoop)
  }

  @tailrec
  private def loopFold[R](
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
		  case Continue(f) => loopFold(charIt, nextIt, restartIt, accum, mapF)
	      case Done(res, EndOfUnit) => loopFold(charIt, restartIt, restartIt, mapF(accum, res._1, res._2), mapF)
	      case Done(res, EndOfInput) => Try(mapF(accum, res._1, res._2))
	      case e @ Error(t) => Failure(t)
	    }
	  case e @ Error(t) => Failure(t)
	  case Done(res, rem) => Try(mapF(accum, res._1, res._2))
	}
  }

  // TODO: this aggregates sequence data into SequenceCachePacked, which currently is only
  // implemented for the DNA alphabet, so any downstream API that uses this (i.e., SuffixTree
  // and DeBruijn graph fromFASTAFile methods) will only work on sequences of DNA seqs
  /*
   * Reifies each sequence in the file and passes it to the combine function, returning the result
   * as an iteratee.
   */
  def enumerateFold[R](seed: R, combine: (R, String, SequenceCache) => R): Iteratee[Char, R] = {
    val compoundIt = for {
	  a <- Iteratees.takeLine		// FASTA sequence header line
	  b <- liftFilter(Iteratee.fold[Char, SequenceCache](new SequenceCachePacked)((r, e) => (r.append(e))))
	} yield (a, b)

    def enumFold(charIt: Iterator[Char]): Iteratee[Char, R] = {
      val resList = loopFold(charIt, compoundIt, compoundIt, seed, combine)
      if (resList.isSuccess) Done(resList.get, EndOfInput)
      else Error(resList.failed.get)
    }

    enumerateT(fileName, enumFold)
  }

  /*
   * Return a list of all sequences in the FASTA file reified in a packed cache (use for
   * DNASequences).
   */
  def reifySequencesPacked: Iteratee[Char, List[(String, SequenceCachePacked)]] =
    enumerateList(Iteratee.fold[Char, SequenceCachePacked](new SequenceCachePacked)((r, e) => (r.append(e))))

  /*
   * Return a list of all sequences in the FASTA file reified in an unpacked cache (use for
   * RNASequences or ProteinSequences).
   */
  def reifySequencesUnpacked: Iteratee[Char, List[(String, SequenceCacheUnpacked)]] =
    enumerateList(Iteratee.fold[Char, SequenceCacheUnpacked](new SequenceCacheUnpacked)((r, e) => (r.append(e))))
}

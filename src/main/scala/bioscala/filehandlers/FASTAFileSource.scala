/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.filehandlers

import scala.annotation.tailrec

//import macros._

import scala.io.BufferedSource
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }

import bioscala.gentypes._

/**
 * FASTA File sequence source: source stream for a single sequence in a FASTA file
 */
class FASTAFileSource(fileName: String) extends FASTAFileParser {

  /*
   * Enumerates the first sequence in a FASTA file.
   */
  def enumerate[R](it: Iteratee[Char, R]): Iteratee[Char, R] = {
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
        @tailrec
	    def loop[B](charIt: Iterator[Char], it: Iteratee[Char, B]): Iteratee[Char, B] = {
	      it match {
	        case Continue(f) => loop(charIt, stepIt(charIt, f, EndOfInput))
	        case o @ other => o
	      }
	    }
	    val tChev = Try{charIt.hasNext && charIt.next == '>'}
	    if (tChev.isSuccess) {
           val resIt = loop(charIt,
              for {
                _ <- Iteratees.takeLine		// eat the FASTA header line
            	b <- liftFilter(it)
              } yield(b))
          tBS.get.close
          tFIS.get.close
          resIt
        }
	    else {
          tBS.get.close
          tFIS.get.close
	      Error(new IllegalArgumentException("Can't parse FASTA file header"))
	    }
      }
    }
  }
}

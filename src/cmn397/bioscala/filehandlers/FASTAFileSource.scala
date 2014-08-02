/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.filehandlers

import scala.annotation.tailrec

import cmn397.Macros._

import scala.io.BufferedSource
import scala.util.control.NonFatal
import scala.util.{ Try, Success, Failure }

import cmn397.bioscala.gentypes._

/**
 * FASTA File sequence source: source stream for a single sequence
 */
// TODO: PARSING - FASTA file sequence uses the filename for the id rather than
// the tag inside the file
class FASTAFileSource(fileName: String) {
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
        val srcIt = tBS.get.iter
	    @tailrec
	    def loop[B](it: Iteratee[Char, B]): Iteratee[Char, B] = {
	      it match {
	        case Continue(f) => {
	          if (srcIt.hasNext) loop(f(Element(srcIt.next)))
	          else f(EndOfInput)
	        }
	        case o @ other => {println("loop other: " + o); o }
	      }
	    }
        // skip over the header line; contents is currently discarded although it should be the sequence ID
        val ret = loop(
          for {
            a <- Iteratees.expect('>')
            b <- Iteratees.takeLine		// eat the FASTA header line
            c <- Iteratees.takeWhile[Char](c => c != '>')
            d <- it
          } yield(b, d)
        )
        tBS.get.close
        tFIS.get.close
        ret match { // strip off the FASTA header result and return only R
          case Done(res, rem) => Done(res._2, rem)
          case Error(ex) => Error(ex)
        }
      }
    }
  }
}

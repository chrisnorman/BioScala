/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.filehandlers

import scala.util.{ Try, Success, Failure }
import scala.io.BufferedSource
import scala.annotation.tailrec
import scala.util.control.NonFatal

import cmn397.bioscala.gentypes._

/**
 * FASTA File sequence source: Source stream for a single sequence
 *
 */
class FASTAFileReader(fileName: String) extends FASTAFileParser {

  // @TODO: this enumerator is identical to the one in FASTAFileSource; this one
  // should enumerate sequences; the other one should enumerate the stream f a single
  // sequence
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
	        case Continue(f) => loop(f(Element(srcIt.next)))
	        case o @ other => o
	      }
	    }
        val headerIt = loop(Iteratees.expect('>').flatMap(it => Iteratees.takeLine))
        if (headerIt.result.isFailure)
          Error(new IllegalArgumentException("Can't parse FASTA file header"))
        else {
          // TODO: reads the rest of the file, INCLUDING other sequences
          val ret = loop(it)
          tBS.get.close
          tFIS.get.close
          ret
        }
      }
    }
  }
}

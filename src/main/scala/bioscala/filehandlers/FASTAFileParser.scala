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
 *
 */
class FASTAFileParser {

  // private sentinel for signaling end of a single sequence to the enumerator
  protected case object EndOfUnit extends Input[Nothing]

  // Strip out embedded cr/lfs
  protected def liftFilter[R](it: Iteratee[Char, R]): Iteratee[Char, R] = {
    it match {
      case Continue(f) =>
        Continue {
          case inp @ Element(e) =>
            if (e == '\r' || e == '\n') liftFilter(it)
            else liftFilter(f(inp))
          case a @ _ => liftFilter(f(a))
        }
      case other @ _ => other
    }
  }

  // fake an end-of-input for the iteratee, but return the value of "eou" param on endOfUnit;
  // this will either be EndOfInput (for FASTAFileSource, since the enumerator should end after
  // the first sequence), or EndOfUnit for FASTAFileReader (since it uses EndOfUnit as a
  // sentinel to start the next sequence with a new seed)
  // TODO: the I/O needs to be inside a TRY
  protected def stepIt[R](charIt: Iterator[Char], f: Input[Char] => Iteratee[Char, R], eou: Input[Nothing]) : Iteratee[Char, R] = {
    if (charIt.hasNext) {
      val c = charIt.next
      //println(c)
	  if (c == '>') {
        f(EndOfInput) match {
          case Done(res, _) => Done(res, eou)
          case other @ _ => other
        }
	  }
	  else f(Element(c))
    }
    else f(EndOfInput)
  }

  protected def enumerateT[R](fileName: String, f: Iterator[Char] => Iteratee[Char, R]): Iteratee[Char, R] = {
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
	    val tChev = Try{charIt.hasNext && charIt.next == '>'}
	    if (tChev.isSuccess) {
	      val getRes = f(charIt)
          tBS.get.close
          tFIS.get.close
          getRes
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

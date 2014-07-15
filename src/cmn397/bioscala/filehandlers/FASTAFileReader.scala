/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.filehandlers

import scala.io.BufferedSource
import scala.annotation.tailrec
import cmn397.bioscala.gentypes._
import scala.util.control.NonFatal


/**
 * FASTA File sequence source: Source stream for a single sequence
 *
 */
class FASTAFileReader(fileName: String) extends FASTAFileParser {

  // @TODO: this enumerator is identical to the one in FASTAFileSource; this one
  // should enumerate sequences; the other one should enumerate the stream f a single
  // sequence
  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = {
    case ite @ _ =>
      try {
        val fis = new java.io.FileInputStream(fileName)
        try {
          val bs = new BufferedSource(fis)
          try {
            val srcIt = bs.iter
            @tailrec
            def loop(ite: Iteratee[Char, R]): Iteratee[Char, R] = {
              ite match {
                case d @ Done(_, _) => d
                case e @ Error(t) => e
                case Continue(f) => loop(nextChar(srcIt, f))
              }
            }
            // @TODO: PARSING - doesn't generate an error on a non-FASTA file- just happily enumerates whatever is there...
            // also should save off this header as the sequence ID
            while (srcIt.hasNext && (srcIt.next != '\n')) {} // skip the first line (header)
            loop(ite)
          } catch { case NonFatal(ex) => Error(ex) }
          finally {
            bs.close
            fis.close
          }
        } catch { case NonFatal(ex) => Error(ex) }
        finally { fis.close }
      } catch { case ex: Exception => Error(ex) }
  }
}

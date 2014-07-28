/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.filehandlers

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.util.control.NonFatal

import cmn397.bioscala.gentypes._

// Enumerator that enumerates sequences: each sequence has an enumerator that handles it's bytes
/**
 * FASTA File sequence source: Source stream for a single sequence
 */

class FASTAFileSource(fileName: String) extends FASTAFileParser {
  /**
   * Creates and returns a FASTAFileSource for the file named by fileName.
   */
  def enumerate[R]: Iteratee[Char, R] => Iteratee[Char, R] = {
    try {
      val fis = new java.io.FileInputStream(fileName)
      try {
        val bs = new BufferedSource(fis)
        try {
          val srcIt = bs.iter
          def loop: Iteratee[Char, R] => Iteratee[Char, R] = {
        	_ match {
              case d @ Done(_, _) => d
              case e @ Error(t) => e
              case Continue(f) =>
                try {
                  loop(nextChar(srcIt, f))
                }
                catch {
                  case NonFatal(ex) => Error(ex)
                }
                finally {
                  bs.close
                  fis.close
                }
              }
          }
          // @TODO: PARSING - doesn't generate an error on a non-FASTA file- just happily enumerates whatever
          // is there...also should save off this header as the sequence ID
          while (srcIt.hasNext && (srcIt.next != '\n')) {} // skip the first line (header)
          loop
        } catch { case NonFatal(ex) => { _ => Error(ex) }}
      } catch { case NonFatal(ex) => { _ => Error(ex) }}
    } catch { case ex: Exception => { _ => Error(ex) }}
  }
}

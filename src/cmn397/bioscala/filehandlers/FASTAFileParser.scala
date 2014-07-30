/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.filehandlers

import cmn397.bioscala.gentypes._

/**
 * 
 */
trait FASTAFileParser {

  // Handles eol characters (\r, \n) and the start of new sequences for FASTA files
  // TODO: can this be eliminated by using Done(_, remaininInput) ??
  protected def nextChar[R](srcIt: Iterator[Char], f: Input[Char] => Iteratee[Char, R]): Iteratee[Char, R] = {
    if (srcIt.hasNext) {
      val nc = srcIt.next
      if (nc == '\r' || nc == '\n') {
        if (srcIt.hasNext) {
          val nc2 = srcIt.next
          if (nc2 == '\n') {
            if (srcIt.hasNext) {
              val nc3 = srcIt.next
              if (nc3 == '>')
                f(Pending) // start of a new sequence
              else
                f(Element(nc3))
            } else
              f(EndOfInput)
          } else if (nc2 == '>')
            f(Pending) // start of a new sequence
          else
            f(Element(nc2))
        } else
          f(EndOfInput)
      } else
        f(Element(nc))
    } else
      f(EndOfInput)
  }
}
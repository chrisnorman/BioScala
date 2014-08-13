/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.gentypes

import scala.util.{ Try, Success, Failure }
import scala.annotation.tailrec

/**
 * Enumerates elements of type E.
 * 
 */
trait Enumerator[E] { self =>

  def enumerate[R](it: Iteratee[E, R]): Iteratee[E, R]
  def apply[R](it: Iteratee[E, R]): Iteratee[E, R] = enumerate(it)

  /*
  def map[U](f: E => Enumerator[U]): Enumerator[U] = {
    new Enumerator[U] {
      def step[R]: Input[E, R]) => Iteratee[U, R] = {
      }
      def enumerate[R]: Iteratee[E, R] => Iteratee[U, R] = self.enumerateStep()
    }
  }
  */
  /*
   * 
  def flatMap[U](f: E => Enumerator[U]): Enumerator[U] = {
  }
  */
}

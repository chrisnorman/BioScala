/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.gentypes

import scala.util.{ Try, Success, Failure }
import scala.annotation.tailrec

// Enumerates elements of type E.
trait Enumerator[E] { self =>

  type Step[E, R] = Iteratee[E, R] => Iteratee[E, R]

  def enumerate[R]: Step[E, R]
  def apply[R](it: Iteratee[E, R]) : Iteratee[E, R] = enumerate(it)

  /*
   * 
   */
  //def map[U](f: E => U): Enumerator[U]

  /*
   * 
   */
  //def flatMap[U](f: E => Enumerator[U]): Enumerator[U]

  def zip[B](rightE: Enumerator[B]) : Enumerator[(Option[E], Option[B])] = {
    def enumerateStep[R](left: Step[E, Option[E]], right: Step[B, Option[B]]):
    		Iteratee[(Option[E], Option[B]), R] => Iteratee[(Option[E], Option[B]), R] = {
      @tailrec
      def eStep(ite: Iteratee[(Option[E], Option[B]), R]): Iteratee[(Option[E], Option[B]), R] = {
        def headOptionIt[T]: Iteratee[T, Option[T]] = {
          def iStep: Input[T] => Iteratee[T, Option[T]] = in =>
            in match {
              case Element(e) => Done(Some(e), Pending)
              case Pending => Done(None, Pending)  		// terminate the enumerator on pending input ??
              case EndOfInput => Done(None, EndOfInput)
            }
            Continue(iStep)
          }
       // TODO: Handle Pending states ?
       ite match {
          case Continue(f) =>
            val leftOpt = left(headOptionIt[E])
            leftOpt match {
              case Done(Some(le), _) =>
              	val rightOpt = right(headOptionIt[B])
              	rightOpt match {
                  case Done(Some(re), _) => eStep(f(Element((Some(le), Some(re)))))
                  case Done(None, _) => eStep(f(Element((Some(le), None))))
                  case Error(t) => Error(t)
                  case other @ _ => Error(new IllegalStateException("enumerator zip rightopt failure"))
              	}
              case Done(_, EndOfInput) =>
              	val rightOpt = right(headOptionIt[B])
              	rightOpt match {
                  case Done(Some(re), _) => eStep(f(Element(None, Some(re))))
                  case Done(None, EndOfInput) => f(Element(None, None))
                  case Error(t) => Error(t)
                  case other @ _ => Error(new IllegalStateException("enumerator zip rightopt(2) failure"))
              	}
              case Error(t) => Error(t)
              case other @ _ => Error(new IllegalStateException("enumerator zip leftopt failure"))
            }
          case od @_ => od
        }
      }
      eStep
    }
    new Enumerator[(Option[E], Option[B])] {
      def enumerate[R]: Iteratee[(Option[E], Option[B]), R] => Iteratee[(Option[E], Option[B]), R] =
    		  	enumerateStep(self.enumerate, rightE.enumerate)
    }
  }
}

/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.gentypes

import scala.util.{ Try, Success, Failure }

// TODO: make sure all cases are covered in each match stmt (especially Done(_, Element(e)))
// Input trait for Iteratee for incrementally processing streams of input.

trait Input[+E] {
  def map[U](f: E => U): Input[U] = this match {
    case Element(e) => Element(f(e))
    case Pending => Pending
    case EndOfInput => EndOfInput
  }
}

case class Element[E](e: E) extends Input[E]
case object Pending extends Input[Nothing]
case object EndOfInput extends Input[Nothing]

trait Iteratee[E, R] {
  // Retrieves the result of the iteratee by pushing an EOF to the continuation.  
  def result: Try[R] = this match { // this is usually called "run" for some reason...
    case Done(res, rem) => Success(res)
    case Error(t) => Failure(t)
    case Continue(k) =>
      k(EndOfInput) match {
        case Done(res, _) => Success(res)
        case Continue(_) => Failure(new Exception("Diverging iteratee"))
        case e @ Error(t) => Failure(t)
      }
  }
  
  def liftInput(inputTransform: E => E): Iteratee[E, R] = this match {
    case Continue(f) =>
      Continue { // map the input using the transform function
        case Element(e) => f(Element(e).map(inputTransform)).liftInput(inputTransform)
        case a @ _ => f(a)
      }
    case other @_ => other
  }

  def showState() = this match {
    case Done(_, Pending) => println("Iteratee: Done pending more input")
    case Done(_, EndOfInput) => println("Iteratee: Done end of input")
    case Error(_) => println("Iteratee: Error")
    case Continue(_) => println("Iteratee: Continue")
    case _ => println("Iteratee: some other outcome")
  }

  // From Play doc: Uses the provided function to transform the Iteratee's computed result when the Iteratee is done.
  def map[U](f: R => U): Iteratee[E, U]
  // From Play doc: On Done of this Iteratee, the result is passed to the provided function, and the resulting Iteratee
  // is used to continue consuming input
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U]
}

//trait Step[E, R] = Input[E] => Iteratee[E, R]

case class Continue[E, R](k: Input[E] => Iteratee[E, R]) extends Iteratee[E, R] {
  def map[U](f: R => U): Iteratee[E, U] = Continue(el => k(el) map f)
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U] = Continue(el => k(el) flatMap f)
}

case class Done[E, R](res: R, remainingInput: Input[E]) extends Iteratee[E, R] {
  //def fold[R](folder: Step[E, R] => folder(Done... James rope slide 14:06
  def map[U](f: R => U): Iteratee[E, U] = Done(f(res), remainingInput) // return this ?
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U] = f(res)
}

case class Error[E, R](t: Throwable) extends Iteratee[E, R] {
  def map[U](f: R => U): Iteratee[E, U] = Error[E, U](t) // return this ?
  def flatMap[U](f2: R => Iteratee[E, U]): Iteratee[E, U] = Error[E, U](t) // return this ?
}

object Iteratee {
  /*
  def apply[E, R](e: Element[E] => Iteratee[E, R],
		  			empty: => Iteratee[E, R],
		  			eof: => Iteratee[E, R]): Iteratee[E, R] = {
    def step(n: Int): Input[A] => Iteratee[A, Int] = {
	    case Element(x)		=> Continue(step(n + 1))
	    case Pending		=> Continue(step(n))
	    case EndOfInput 	=> Done(n, EndOfInput)
	}
	Continue(step(0))
  }
  def fold[E, R](z: R)(f: (R, E) => R): Iteratee[E, R] = {}
*/
}

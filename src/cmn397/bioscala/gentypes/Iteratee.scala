/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.gentypes

import scala.util.{ Try, Success, Failure }

// TODO: Future
import scala.concurrent.Future

// TODO: Input trait for Iteratee for incrementally processing streams of input.

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

  /**
   * Retrieves the result of this Iteratee/computation. If necessary, it will push an EOF to
   * the continuation to force the result.
   * 
   * NOTE: if there is remaining input when this is called, the iteratee can still consume
   * more input starting with that remaining input.
   */
  def result: Try[R] = this match { // this is usually called "run"...
    case Done(res, rem) => Success(res)
    case Error(t) => Failure(t)
    case Continue(k) =>
      k(EndOfInput) match {
        case Done(res, _) => Success(res)
        case Continue(_) => Failure(new Exception("Diverging iteratee"))
        case e @ Error(t) => Failure(t)
      }
  }
  
  /**
   * Returns an Iteratee that maps the values from the enumerator BEFORE they are passed to the
   * input handler/continuation (as opposed to mapping the Iteratee itself, which maps the computational
   * result of the Iteratee after the fact).
   */
  // TODO: is this rendered obsolete by Enumeratee ??
  def mapInput(inputTransform: E => E): Iteratee[E, R] = this match {
    case Continue(f) =>
      Continue { // map the input using the transform function
        case Element(e) => f(Element(e).map(inputTransform)).mapInput(inputTransform)
        case a @ _ => f(a)
      }
    case other @_ => other
  }

  /**
   * Debugging: display the state of this Iteratee on stdout.
   */ 
 def showState() = this match {
    case Done(_, Pending) => println("Iteratee: Done pending more input")
    case Done(_, EndOfInput) => println("Iteratee: Done end of input")
    case Error(_) => println("Iteratee: Error")
    case Continue(_) => println("Iteratee: Continue")
    case _ => println("Iteratee: some other outcome")
  }

  /**
   * Transform the Iteratee's computed result when the Iteratee is Done.
   */
  def map[U](f: R => U): Iteratee[E, U]

  /**
   * The result of the completed computation of this Iteratee (R), is passed to the function f
   * and the resulting Iteratee can continue consuming input.
   */
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U]
}


case class Continue[E, R](k: Input[E] => Iteratee[E, R]) extends Iteratee[E, R] {
  def map[U](f: R => U): Iteratee[E, U] 					= Continue(el => k(el) map f)
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U] 	= Continue(el => k(el) flatMap f)
}

case class Done[E, R](res: R, remainingInput: Input[E]) extends Iteratee[E, R] {
  //def fold[R](folder: Step[E, R] => folder(Done... James Rope slide 14:06
  def map[U](f: R => U): Iteratee[E, U] = Done(f(res), remainingInput)
  def flatMap[U](f: R => Iteratee[E, U]): Iteratee[E, U] = f(res)
}

case class Error[E, R](t: Throwable) extends Iteratee[E, R] {
  def map[U](f: R => U): Iteratee[E, U] = Error(t)
  def flatMap[U](f2: R => Iteratee[E, U]): Iteratee[E, U] = Error(t)
}

object Iteratee {

  /**
   * Create a basic Iteratee with simple Done/Error processing, parameterized with a continuation
   * function "f" that will handle the (R, E) => R (input to new state) transformation.
   * 
   * NOTE: the iteratee terminates the enumerator when the continuation receives "Pending" input.
   * TODO: Not sure if terminating on Pending is the right thing....
   */

  def fold[E, R](state: R)(f: (R, E) => R): Iteratee[E, R] = {
    def step(r: R): Input[E] => Iteratee[E, R] = in =>
      in match {
        case Element(e) => Continue(step(f(r, e)))
        case Pending => Done(r, Pending)  // terminate the enumerator on pending input ??
        case EndOfInput => Done(r, EndOfInput)
      }
    Continue(step(state))
  }

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
*/
}

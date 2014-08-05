/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package cmn397.bioscala.gentypes

/**
 * State action
 * 
 */
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State ({
      s =>
          val (a, newState) = run(s)
          f(a).run(newState)
      })
  }

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
}

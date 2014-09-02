/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.graph

import bioscala.core.Alphabet
import bioscala.graph._

/**
 * Types used by Finite State Machine implementation.
 */
object FSM {
  type State = Int
  val InvalidState = -1

  /*
   * The simple pattern language recognized by this converter is:
   * 
   *   any letter in the (target) alphabet (though this is not currently enforced)
   *   {...} to indicate any one of
   *   [...] to indicate any letter except those listed
   */
  def fromPattern[T](p: Vector[T], alphabet: Alphabet[T]): FSM[T] = {
    def loop(acc: FSM[T], pat: Vector[T], curState: Int): FSM[T] = {
      def addTransition[T](st: (FSM[T], Int), c: T): (FSM[T], Int) = (st._1.addTransition(curState, st._2, c), st._2)
      if (pat.isEmpty) acc.addAcceptState(curState)
      else {
        pat.head match {
          case '{' =>
		            val exceptList = pat.tail.takeWhile(_ != '}')
		            val (newAcc1, newState) = acc.addState(false)
		            val acceptList = alphabet.filter(!exceptList.contains(_))
		            val (newAcc2 , nextState) = acceptList.foldLeft((newAcc1, newState))(addTransition)
		            loop(newAcc2, pat.dropWhile(_ != '}'), nextState)

          case '}' => loop(acc, pat.tail, curState)

          case '[' =>
		            val orList = pat.tail.takeWhile(_ != ']')
		            val (newAcc1, newState) = acc.addState(false)
		            val (newAcc2 , nextState) = orList.foldLeft((newAcc1, newState))(addTransition)
		            loop(newAcc2, pat.dropWhile(_ != ']'), nextState)

          case ']' => loop(acc, pat.tail, curState)

          case c@ _ =>
		            val (newAcc1, newState) = acc.addState(false)
		            val newAcc2 = newAcc1.addTransition(curState, newState, c)
		            loop(newAcc2, pat.tail, newState)
        }
      }
    }
    loop(new FSM[T](alphabet), p, 0)
  }
}

/**
 * Finite State Machine
 * 
 * Type parameter T represents the data type of a transition event (i.e., Char)
 * State 0 is the start state; acceptStates is a list of all accepting (end) states
 * 
 */
class FSM[T] private[FSM] (alphabet: Alphabet[T], states: Vector[Vector[FSM.State]], acceptStates: Vector[Int]) {

  // FSM construction
  def this(alphabet: Alphabet[T]) = this(alphabet, Vector[Vector[FSM.State]](Vector[FSM.State]().padTo(alphabet.length, -1)), Vector[FSM.State]())

  // @FIX: this returns a single state value in the pair but its adding multiple states/
  /**
   * Add a list of new states to the existing machine. Returns a pair representing the new FSM instance
   * and the id of the newly added state.
   */
  private def addState(st: Vector[FSM.State], isAccept: Boolean = false): (FSM[T], Int) = {
    val newFSM = new FSM[T](
      alphabet,
      states :+ st,
      if (isAccept) acceptStates :+ states.length else acceptStates)
    (newFSM, states.length)
  }

  /**
   * Add a new state to the existing machine. Returns a pair representing the new FSM instance
   * and the ID of the newly added state.
   */
  def addState(isAccept: Boolean): (FSM[T], Int) = {
    val v = Vector().padTo(alphabet.length, -1)
    addState(v, isAccept)
  }

  /**
   * Add a new state transition to the machine.
   * 
   * @param e : the event (from the Alphabet for this FSM) representing the transition
   * 
   */
  def addTransition(fromState: FSM.State, toState: FSM.State, e: T): FSM[T] = {
    require(fromState < states.length && toState < states.length)
    val i = alphabet.indexOf(e)
    require(i != -1)
    new FSM(alphabet, states.updated(fromState, states(fromState).updated(i, toState)), acceptStates)
  }
  
  /**
   * Add an existing state to the list of accept states for this machine.
   */
  def addAcceptState(st: FSM.State) = {
    require (st < states.length)
    new FSM[T](alphabet, states, acceptStates :+ st)
  }

  /**
   * Display the FSM on the console.
   */
  def display: Unit = {
    println("Alphabet: " + alphabet)
    println("States: " + states)
    println("Accept States: " + acceptStates)
  }

  // FSM execution
  def isValidState(state: FSM.State) = state != -1
  def isAcceptState(state: FSM.State) = acceptStates.contains(state)
  def getValidEvents(state: FSM.State) = states(state).zip(states(state).indices).filter(_._1 != -1).map(e => alphabet.symbols(e._2))
  def getNextState(state: FSM.State, e: T) : Int = {
    val i = alphabet.indexOf(e)
    require(i != -1)
    states(state)(i)
  }
}

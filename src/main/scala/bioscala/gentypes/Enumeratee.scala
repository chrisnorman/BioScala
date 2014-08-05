/**
 *
 * Copyright (c) 2014 Chris Norman
 *
 * @author Chris Norman (cmn397@gmail.com)
 *
 */

package bioscala.gentypes

/**
 * Combines the roles of an Iteratee[From] and a Enumerator[To].  This allows adapting of streams to that modify input
 * produced by an Enumerator, or to be consumed by a Iteratee.
 */
/*
object Enumeratee {
  def map[A](f: From => A): Enumeratee[From, To] 
}
*/

/**
 * 
 */
trait Enumeratee[From, To] {
//create am Enumeratee using the map method on Enumeratee
//val toInt: Enumeratee[String,Int] = Enumeratee.map[String]{ s => s.toInt }
//val adaptedIteratee: Iteratee[String,Int] = toInt.transform(sum)

  /**
   * Create a new Iteratee that feeds its input, potentially modifying it along the way, into the inner Iteratee, and
   * produces that Iteratee as its result.
   */
  def applyOn[A](inner: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]]

  /**
   * Transform the given iteratee into an iteratee that accepts the input type that this enumeratee maps.
   */
  def transform[A](inner: Iteratee[To, A]): Iteratee[From, A]
}
/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package cmn397.bioscala.core

/**
 * Base class for DNAAlphabet, RNAAlphabet and ProteinAlphabet
 * 
 * @tparam T: the data type for a symbol in the alphabet. For sequence alphabets this will
 * usually be type 'Char', but it can also represent other types (i.e. to support an event
 * alphabet for a Finite State Machine).
 */
class Alphabet[T](val symbols: Vector[T]) {

  /** Returns the index of an individual symbol in the alphabet. */
  def indexOf(c: T) = symbols.indexOf(c)

  /** Returns a filterec vector of symbols in the alphabet. */
  def filter(f: T => Boolean) : Vector[T] = symbols.filter(f)

  /** Number of symbols in the alphabet. */
  val length : Int = symbols.length

  override def toString: String = symbols.mkString
}

/** An alphabet for DNASequences */
object DNAAlphabet extends Alphabet[Char]("ACGT".toVector)

/** An alphabet for RNASequences */
object RNAAlphabet extends Alphabet[Char]("ACGU".toVector)

/** An alphabet for ProteinSequences */
object ProteinAlphabet extends Alphabet[Char]("ACDEFGHIKLMNPQRSTVWY".toVector)

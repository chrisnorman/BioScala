/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.graph

import scala.util.Try
import scala.collection.BitSet
import scala.annotation.tailrec

import bioscala.core._
import bioscala.filehandlers.FASTAFileReader

object SuffixTree {

  // TODO: fromFASTAFile uses FASTAFileReader, which in turn uses a packed cache, which assumes DNASequences
  def fromFASTAFile(fName: String): Try[SuffixTree] = {
    val ffr = new FASTAFileReader(fName)
    ffr.enumerateResult(
    	    new SuffixTree(),
    	    (oldState: SuffixTree, id: String, c: SequenceCache) => {
    	      	oldState.updated(new DNASequence(id, new SequenceSourceCache(c)))
    	    }
    ).result
  }
}

/**
  * Base class for a SuffixTree node.
  */
private abstract class STNode(val path: String) {

  @tailrec
  private[graph] final def addSuffixes(chars: String, id: String, loc: Long): STNode = {
    if (chars.isEmpty) this
    else addSuffix(chars, id, loc).addSuffixes(chars.tail, id, loc + 1)
  }

  /*
   * Add a suffix to the suffix tree.
   * 
   * Note: There are two implementations of this method in subclasses which are mutually recursive
   * but the recursion depth will remain shallow since the STBranch implementation is tail recursive.
   * 
   */
  def addSuffix(chars: String, id: String, loc: Long): STNode


  /**
   * Return the index of the last matching character in s1 and s2.
   */
  @tailrec
  protected final def extendSuffixMatch(s1: String, s2: String, pos: Int) : Int = {
    if (s1.isEmpty || s2.isEmpty || s1.head != s2.head) pos
    else extendSuffixMatch(s1.tail, s2.tail, pos + 1)
  }

  /**
   * Display/debugging
   */
  private[graph] def showContents(n: Int) : Unit
  private[graph] def countNodes: Long
}

/**
  * SuffixTree leaf node.
  * 
  */
private[graph] case class STLeaf(override val path: String, val sources: List[(String, Long)]) extends STNode(path) {

  override def addSuffix(suffix: String, id: String, loc: Long) : STNode = {
    val matchEnd = extendSuffixMatch(suffix, path, 0)
    val (pathPrefix, pathSuffix) = path.splitAt(matchEnd)
    require(pathSuffix.length != 0) 	// should never matched the entire leaf path due to addition of the terminator (i.e. $)
    if (matchEnd == suffix.length)		// we matched the entire suffix, just add the new source to this leaf
      new STLeaf(path, (id, loc) :: sources)
    else {
      // split this node into a branch with a copy of this leaf as a child, and delegate
      // the rest of the suffix to the branch
      val branch = new STBranch(pathPrefix, Vector(new STLeaf(pathSuffix, sources)))
      branch.addSuffix(suffix, id, loc)  // pass the entire suffix and let it match in the new branch
    }
  }

  /**
   * Display/debugging
   */
  private[graph] override val countNodes = 1L
  private[graph] override def showContents(n: Int) = {
    val pad = List[Char]().padTo(n, ' ').mkString 
    println(pad + path + " Sources: " + sources.mkString)
  }
}

/**
  * SuffixTree branch node.
  * 
  */
private[graph] case class STBranch(override val path: String, val nodes: Vector[STNode]) extends STNode(path) {

  @tailrec
  final override def addSuffix(suffix: String, id: String, loc: Long): STBranch = {
    if (suffix.isEmpty) this
    else {
      val matchEnd = extendSuffixMatch(suffix, path, 0)
      val (pathPrefix, pathSuffix) = path.splitAt(matchEnd)
      val (newPathFragment, remainingSuffix) = suffix.splitAt(matchEnd)
	  require(newPathFragment.mkString == pathPrefix)
	  if (pathSuffix.length == 0) { // the new suffix matches the whole path fragment, delegate the remainder to children
	    require(pathPrefix == path)
        nodes.find(_.path(0) == remainingSuffix.head) match {
	      case Some(n) => // delegate adding the suffix to the matching node, and replace the old node with the new one
	    				  new STBranch(path, nodes.filter(_ != n) :+ n.addSuffix(remainingSuffix, id, loc))
	      case None	 => // add a brand new leaf for the remainder of the new suffix
	      	  		    new STBranch(path, nodes :+ new STLeaf(remainingSuffix.mkString, List((id, loc))))
	    }
	  }
	  else {
	    // the new suffix matches part of this node, split this node into a new branch that represents
	    // the common path; and delegate the remainder of the new suffix to the branch
	    require(newPathFragment.length != 0 && remainingSuffix.length != 0)
	    val newChild = new STBranch(pathSuffix, nodes)
	    val newParent = new STBranch(pathPrefix.mkString, Vector(newChild))
	    newParent.addSuffix(suffix, id, loc)
	  }
    }
  }

  /**
   * Display/debugging
   */
  override def countNodes: Long = nodes.foldLeft(1L)((acc: Long, nd: STNode) => acc + 1L + nd.countNodes)
  override private[graph] def showContents(n: Int) : Unit = {
    val pad = List[Char]().padTo(n, ' ').mkString 
	println(pad + path)
	for (nd <- nodes) {
	  nd.showContents(n + 1)
	}
  }    
}

// TODO: implement a suffix array and/or us a packed cache of some kind to store the path strings
// as the current implementation is unrealistic for actual sequences that are large.
/** 
 * Generalized Suffix Tree.
 * 
 * Naive implementation of a Generalized Suffix Tree for multiple strings. Uses McCreight-style path
 * compression to eliminate internal nodes with a single character path.
 *
 * NOTE:
 *    Each string in the tree must be terminated (by the caller!) with a character that is:
 *
 *  	1) unique (no other string in the tree can be terminated with that character) and:
 *   	2) the character should not be found in the alphabet from which the strings are otherwise
 *         drawn (or put another way, the termination character should not appear anywhere else
 *         in any of the strings)
 *  
 * Example usage:
 *  
 *  	val st0 = new graph.SuffixTree
 *  	val st1 = st0.updated("GATTACA$", "id1")
 *  	val st2 = st1.updated("TAGACCA#", "id2")
 *   	val lst = st3.LongestCommonSubstring // Longest Common Substring (as a list - there may be more than one) 
 *
 */
class SuffixTree protected(private val root: STNode, val terminator: Int) {
  def this() = this(new STBranch("", Vector()), 33) // 33 is the beginning of the asci range used for auto terminator generation
  def updated(str: String, id: String): SuffixTree = new SuffixTree(root.addSuffixes(str, id, 0), terminator)
  def updated(seq: Sequence): SuffixTree = {
	// TODO: since the sequences in FASTA files don't have unique termination characters (which are required
    // by the suffix tree algorithm), this is a hack to auto-generate a terminator from the ascii range from 33-47
    // After that, it just throws..
    if (terminator > 47)
      throw new java.lang.IllegalStateException("Exceeded limit of auto termination-character-generation hack")
    else
      new SuffixTree(root.addSuffixes(seq.asString(None) + terminator.toChar, seq.id, 0), terminator+1)
  }
  def display: Unit = root.showContents(0)
}

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

/*
 * TODO: The tree building implementation is naive (read: O(n**2)). The first node is added optimistically
 *  	 by adding a single edge terminated with a single leaf, and subsequent strings are added by traversing
 *   	 the existing tree and splitting any existing leaves/SuffixTreees to create appropriate new nodes. Ukkonen's
 *       algorithm might be a better choice ("online" and O(n)).
 * TODO: Need to prevent the addition of more suffixes once we've computed the annotations phase
 *  	 since it invalidates them. Or else just recalculate them every time we call LCS, which is what
 *   	 the code currently does. A related change: don't clone on LCS if we've already calculated the
 *       annotations and nothing has changed.
 */

object SuffixTree {

  /**
   * Returns a SuffixTree  representing the sequence strings in the FASTA file fName.
   *
   * NOTE: this currently assumes the FASTA file contains a DNA sequence
   */
  def fromFASTAFile(fName: String): Try[SuffixTree] = {
    val ffr = new FASTAFileReader(fName)
    ffr.enumerateResult(
    	    new SuffixTree(),
    	    (acc: SuffixTree, id: String, c: SequenceCache) => {
    	      	acc.updated(new DNASequence(id, new SequenceSourceCache(c)))
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

  // Annotations and helpers
  private[graph] def createSourceMap(m: Map[String, Int]) : Map[String, Int]
  private[graph] def createAnnotations(m: Map[String, Int]) : BitSet
  private[graph] def getAnnotations: BitSet
  private[graph] def cloneWithAnnotations(m: Map[String, Int]) : STNode

  // String matching methods (LCS, etc.) and related helpers
  private[graph] def findLCSCandidates(bs: BitSet, path: String, acc: List[String]) : List[String]
  private[graph] def findKMEROccurrences(k: Int, bs: BitSet, path: String, acc: List[(String, Long)]) : List[(String, Long)]
  private[graph] def findPatternMatch(acc: List[(Long, Long)], fsm: FSM[Char], curState: FSM.State, matchLen: Long) : List[(Long, Long)]
	
  // Helpers
  private[graph] def countDescendants(acc: Long): Long // get count of # of sources in all descendant leaves

  // TODO: this is just a fold....
  // Visit each leaf and return a flat list of results returned by visitf 
  private[graph] def visitAllLeaves[T](acc: List[T], n: STNode, visitf: STLeaf => List[T]) : List[T] = {
	n match {
	  case st: STBranch => st.nodes.foldLeft(acc)((acc, nd) => visitAllLeaves(acc, nd, visitf) ::: acc)
	  case lf: STLeaf => visitf(lf) ::: acc
	}
  }

  // TODO: this is just a fold....
  // Visit each leaf and aggregate the return values using the combine function
  private[graph] def visitAllLeavesCombine[T](acc: T, n: STNode, visitF: STLeaf => T, combineF: (T, T) => T) : T = {
    n match {
      case st: STBranch => st.nodes.foldLeft(acc)((acc, nd) => combineF(visitAllLeavesCombine(acc, nd, visitF, combineF), acc))
      case lf: STLeaf => combineF(visitF(lf), acc)
    }
  }

  def lcs: List[String] = List()
  def mostFrequentKmers(k: Int): List[(String, Long)] = List()
  def substringsFromPattern[T](alphabet: Alphabet[Char], pattern: String): List[(Long, Long)] = List()

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
    require(pathSuffix.length != 0) // should never matched the entire leaf path due to the unique terminators (i.e. $)
    if (matchEnd == suffix.length)	// entire suffix matches, just add the new source to this leaf
      new STLeaf(path, (id, loc) :: sources)
    else {	// Split this node into a branch with a copy of this leaf as a child,
    		// and delegate the rest of the suffix to the branch
      val branch = new STBranch(pathPrefix, Vector(new STLeaf(pathSuffix, sources)))
      branch.addSuffix(suffix, id, loc)  // pass the entire suffix and let it match in the new branch
    }
  }

    private[graph] def findPatternMatch(acc: List[(Long, Long)], fsm: FSM[Char], curState: FSM.State, matchLen: Long): List[(Long, Long)] = {
    def loop(acc: List[(Long, Long)], fsm: FSM[Char], curState: FSM.State, str: List[Char], matchLen: Long) : List[(Long, Long)] = {
      if (str.isEmpty) acc
      else {
        val candidateEvents = fsm.getValidEvents(curState)
        if (candidateEvents.contains(str.head)) {
		  val nextState = fsm.getNextState(curState, str.head)
		  require(fsm.isValidState(nextState))
		  if (fsm.isAcceptState(nextState)) {
		    val newSources = sources.map(e => (e._2, matchLen + 1))
		    newSources ::: acc 
		  }
		  else loop(acc, fsm, nextState, str.tail, matchLen + 1)
        }
		else acc
      }
    }
    loop(acc, fsm, curState, path.toList, matchLen)
  }

  private[graph] def createSourceMap(m: Map[String, Int]) : Map[String, Int] = {
    sources.foldLeft(m)((m: Map[String, Int], v: (String, Long)) =>
     	if (m.contains(v._1)) m else m + (v._1 -> (m.size + 1)))
  }

  private[graph] def getAnnotations: BitSet = BitSet()
  private[graph] def createAnnotations(m: Map[String, Int]) : BitSet =
		  sources.foldLeft(BitSet())((acc: BitSet, source: (String, Long)) => acc | BitSet(m(source._1)))
  private[graph] def cloneWithAnnotations(m: Map[String, Int]): STLeaf = this

  private[graph] def findLCSCandidates(b: BitSet, path: String, acc: List[String]) : List[String] = acc
  private[graph] def findKMEROccurrences(k: Int, bs: BitSet, path: String, acc: List[(String, Long)]) : List[(String, Long)] = acc

  private[graph] def countDescendants(acc: Long): Long = { // get count of # of sources in all descendant leaves
	acc + sources.length
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
private[graph] case class STBranch(
    override val path: 	String,
    val nodes: 			Vector[STNode],
    val annotation: 	BitSet = BitSet()
  ) extends STNode(path)
{
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


  // TODO: currently this code (and is downstream helpers) assume that there is only one string in the tree and they do not
  // return the string ID, though they should
  private[graph] def findPatternMatch(acc: List[(Long, Long)], fsm: FSM[Char], curState: FSM.State, matchLen: Long): List[(Long, Long)] = {
	def loop(acc: List[(Long, Long)], fsm: FSM[Char], l: List[Char], curState: FSM.State, matchLen: Long) : List[(Long, Long)] = {
	  val candidateEvents = fsm.getValidEvents(curState)
	  if (l.isEmpty) { 	// we've matched this entire path fragment, delegate to child nodes
	    val candidateNodes = nodes.filter(n => candidateEvents.contains(n.path(0)))
	    candidateNodes.foldLeft(acc)((acc, nd) => nd.findPatternMatch(acc, fsm, curState, matchLen))
	  }
	  else {
		if (candidateEvents.contains(l.head)) {
		  val nextState = fsm.getNextState(curState, l.head)
		  if (fsm.isAcceptState(nextState)) {
			val sList = visitAllLeaves(List[(String, Long)](), this, (leaf) => leaf.sources)
			 sList.foldLeft(acc)((acc, pair) => (pair._2, matchLen + 1) :: acc)
		  }
		  else loop(acc, fsm, l.tail, nextState, matchLen + 1)
		}
		else acc
	  }
	}
	loop(acc, fsm, path.toList, curState, matchLen)
  }

  /**
   * Populate a map representing all of the various sources found in the leaves of this tree, with each
   * one assigned to a unique value (used to annotate SuffixTree nodes with the list of sources represented
   * in it's descendant leaves).
   */
  private[graph] def createSourceMap(m: Map[String, Int]) : Map[String, Int] =
    nodes.foldLeft(m)((acc: Map[String, Int], n: STNode) => n.createSourceMap(acc))

  private[graph] def cloneWithAnnotations(m: Map[String, Int]): STNode = {
	val nds = nodes.map(_.cloneWithAnnotations(m))
	val bs = nodes.foldLeft(BitSet())((acc: BitSet, n: STNode) => acc | n.createAnnotations(m))
	new STBranch(path, nds, bs)
  }

  private[graph] def getAnnotations: BitSet = annotation
  private[graph] def createAnnotations(m: Map[String, Int]) : BitSet = {
	  nodes.foldLeft(BitSet())((acc: BitSet, n: STNode) => acc | n.createAnnotations(m))
  }

  /**
   * The current strategy for finding the LCS is to create a new tree with the proper annotations (each branch
   * node/SuffixTree has a list which is the union of all id/source labels seen in any leaf node descendant), then
   * traverse the new tree looking for internal nodes that have an annotation that matches the subset of targets
   * we're looking for (which is usually all of the strings used in the creation of the tree). The path to any
   * such node represents a substring common to all of those sources; the LCS is the longest of these.
   */
  override def lcs: List[String] = {
	val m = createSourceMap(Map())							// collect in a map all of the IDs that appear in any Leaf
	val bs = BitSet() ++ (1 to m.size)						// create a bitset representing all of these IDs
	val st = cloneWithAnnotations(createSourceMap(Map()))	// create a new tree containing the old tree with the annotations
	val candidates = st.findLCSCandidates(bs, "", List())	// find the list of candidate strings based on the annotations
	val mLen = candidates.foldLeft(0L)((acc: Long, s: String) => if (acc > s.length) acc else s.length)
	candidates.filter(_.length == mLen)						// and find the longest ones
  }
  
  /*
   * Determine if a string (represented by the target suffix tree) contains the motif described
   * by a pattern string, where [XY] in the pattern means either X or Y and {X} means any character
   * except X. Returns the location and length of the substring that matches the pattern.
   */
  override def substringsFromPattern[T](alphabet: Alphabet[Char], pattern: String) : List[(Long, Long)] = {
	val pat = pattern.toVector
	val fsm = FSM.fromPattern(pat, alphabet)
	val st = cloneWithAnnotations(createSourceMap(Map()))	// create a new tree containing the old tree with the annotations
	st.findPatternMatch(List[(Long, Long)](), fsm, 0, 0)
  }

  /*
   * Private helpers
   */
  private[graph] def findLCSCandidates(bs: BitSet, path: String, acc: List[String]) : List[String] = {
	def f(accum: List[String], nd: STNode) = nd.findLCSCandidates(bs, path + nd.path, accum)
	val newlist = if (((bs & annotation) == bs) && path.length > 0) path :: acc else acc
	nodes.foldLeft(newlist)(f)
  }

  /**
   */
  override def mostFrequentKmers(k: Int) : List[(String, Long)] = {		// return value is list of (k-mer, occurrence-count)
	val m = createSourceMap(Map())								// collect in a map all of the IDs that appear in any Leaf
	val bs = BitSet() ++ (1 to m.size)							// create a bitset representing all of these IDs
	val st = cloneWithAnnotations(createSourceMap(Map()))		// create a new tree containing the old tree with the annotations
	val candidates = st.findKMEROccurrences(k, bs, "", List())	// find the list of candidate strings based on the annotations
	val mLen = candidates.foldLeft(0L)((acc: Long, p: (String, Long)) => if (acc > p._2) acc else p._2)
	candidates.filter(_._2 == mLen)								// and find the one with the greatest # of occurrences
  }

  private[graph] def findKMEROccurrences(k: Int, bs: BitSet, path: String, acc: List[(String, Long)]) : List[(String, Long)] = {
	def f(accum: List[(String, Long)], nd: STNode) = {
	val locPath = path + nd.path
	val len = locPath.length
	  if (len < k)						// search deeper nodes
		nd.findKMEROccurrences(k, bs, locPath, accum)
	  else {
		val occurrences = nd.countDescendants(0)
		if (len > k) (locPath.take(k), occurrences) :: accum  	// grab up to k chars
		else (locPath, occurrences) :: accum					// grab the entire path      
	  }
	}
	nodes.foldLeft(acc)(f)
  }

  private[graph] def countDescendants(acc: Long): Long = { // get count of # of sources in all descendant leaves
	nodes.foldLeft(acc)((accum: Long, nd: STNode) => nd.countDescendants(accum))
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
  def this() = this(new STBranch("", Vector()), 33) // 33 is the beginning of the ascii range used for auto terminator generation
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

  def lcs: List[String] = root.lcs
  def mostFrequentKmers(k: Int): List[(String, Long)] = root.mostFrequentKmers(k)
  def matchPattern[T](alphabet: Alphabet[Char], pattern: String): List[(Long, Long)] = {
    root.substringsFromPattern(alphabet, pattern)
  }

  def display: Unit = root.showContents(0)
}

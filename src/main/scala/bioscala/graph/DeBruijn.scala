/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.graph

import scala.util.Try

import bioscala.core._
import bioscala.filehandlers.FASTAFileReader

/*
 * DeBruijn graph creation methods.
 */
object DeBruijn {

  // TODO: fromFASTAFile uses FASTAFileReader, which in turn uses a packed cache, which assumes DNASequences

  /**
   * Returns a DeBruijn graph representing the sequence strings in the FASTA file fName.
   */
  def fromFASTAFile(fName: String, k:Int): Try[DeBruijn] = {
    val ffr = new FASTAFileReader(fName)
    ffr.enumerateResult(
    	    new DeBruijn(k),
    	    (oldState: DeBruijn, edgeLabel: String, c: SequenceCache) => {
    	      	oldState.addVertex(edgeLabel, new SequenceSourceCache(c).asString())
    	    }
    ).result
  }

  /**
   * Returns a DeBruijn graph representing the sequence strings in a list.
   */ 
  def fromSequenceList(ls: List[Sequence], k: Int): DeBruijn = {
    def accGraph(ls: List[Sequence], graph: DeBruijn): DeBruijn = {
      if (ls.isEmpty) graph
      else accGraph(ls.tail, graph.addVertex(ls.head.id, ls.head.asString(None)))
    }
    accGraph(ls, new DeBruijn(k))
  }
}

private case class Vertex(s: String)
private case class Edge(s: String, label: String)

/**
 * DeBruijn graph implementation for finding overlaps.
 * 
 * 	Vertex: k-mer prefix or suffix
 *  Edge: k-mer, k is the prefix/suffix length
 * 
 */
class DeBruijn private(k: Int, vertexMap: Map[String, Vertex], edgeMap: Map[String, (Vertex, Vertex, String)]) {
  require(k > 1)

  def this(k: Int) = {
    this(k, Map(), Map())
  }

  /**
   * Add a new vertex to an existing graphs. Returns a new DeBruijn graph instance.
   */
  def addVertex(edgeLabel: String, s: String): DeBruijn = {
    if (edgeMap.contains(s)) this
    else {
      val prefix = s.take(k)
      val suffix = s.takeRight(k)
      val pv = vertexMap.getOrElse(prefix, new Vertex(prefix))
      val sv = vertexMap.getOrElse(suffix, new Vertex(suffix))
      val vm = vertexMap.updated(prefix, pv).updated(suffix, sv)
      new DeBruijn(k, vm, edgeMap.updated(s, (pv, sv, edgeLabel)))
    }
  }

  /**
   * Finds all of the overlap pairs in the graph.
   */
  def findOverlapPairs:List[(String, String)] = {
    for {
      key <- edgeMap.keys.toList
      val (kString, kEdge, kLabel) = edgeMap(key)
      j <- edgeMap.keys
      if (key != j)
      val (jString, jEdge, jLabel) = edgeMap(j)
      if (kEdge == jString)
    } yield (kLabel, jLabel)
  }

  /**
   * Displays the graph on the console.
   */
  def degbugDisplay: Unit = {
    println("DeBruijn Graph:\nVertices")
    println(vertexMap)
    println("Edges")
    for (k <- edgeMap.keys)
      println(k + edgeMap(k).toString)
  }
}

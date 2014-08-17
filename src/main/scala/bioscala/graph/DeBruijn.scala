/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.graph

// Start with empty DeBruijn
// For each sequence
//    collect the entire string
//	  add to DeBruijn graph

//import bioscala.filehandlers.FASTAFileReader

/*
object DeBruijn {
  /**
   * Returns a DeBruijn graph representing the sequence strings in the FASTA file fName.
   */
  def fromFASTAFile(fName: String, k:Int): DeBruijn = {
    val ffr = new FASTAFileReader(fName)
    val it = Iteratee.fold[Char, Try[DeBruijn]](Try(new DeBruijn))
    	((r, e) => )

    val res = ffr.enumerate(it)
    /*
    def accGraph(l: List[DNASequence], g: DeBruijn): DeBruijn = {
      if (l.isEmpty) g
      else accGraph(l.tail, g.addVertex(l.head.getS.mkString, l.head.id))
    }
    accGraph(l, new DeBruijn(k))
    */
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
  def addVertex(s: String, label: String): DeBruijn = {
    // FIX: is it an error if this k-mer already exists in the graph ?
    if (edgeMap.contains(s)) this
    else {
      val prefix = s.take(k)
      val suffix = s.takeRight(k)
      val pv = vertexMap.getOrElse(prefix, new Vertex(prefix))
      val sv = vertexMap.getOrElse(suffix, new Vertex(suffix))
      val vm = vertexMap.updated(prefix, pv).updated(suffix, sv)
      new DeBruijn(k, vm, edgeMap.updated(s, (pv, sv, label)))
    }
  }

  /**
   * Finds all of the overlap pairs in the graph.
   */
  def findOverlapPairs:List[(String, String)] = {
    for {
      k <- edgeMap.keys.toList
      val (ks, ke, kl) = edgeMap(k)
      j <- edgeMap.keys
      if (k != j)
      val (js, je, jl) = edgeMap(j)
      if (ke == js)
    } yield (kl, jl)
  }

  /**
   * Displays the graph on the console.
   */
  def display: Unit = {
    println("DeBruijn Graph:\nVertices")
    println(vertexMap)
    println("Edges")
    for (k <- edgeMap.keys)
      println(k + edgeMap(k).toString)
  }
}
*/
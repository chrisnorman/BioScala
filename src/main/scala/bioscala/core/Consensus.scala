/**
 *
 * Copyright (c) 2014 Chris Norman
 * 
 * @author Chris Norman (cmn397@gmail.com)
 * 
 */

package bioscala.core

// TODO: make this work on any sequence alphabet (instead of just DNASequence)

 /*
 * Contains implementation of *DNA* sequence profile and consensus string finding.
 */
object Consensus {

  /**
   * Return a profile matrix and consensus string for a series of aligned DNA sequences.
   *
   */
  def getConsensus(seqL: List[DNASequence]): (List[(Long, Long, Long, Long)], List[Char]) = {
    val profile = generateProfile(seqL.map(s => s.iterator)); 
    (profile, consensus(profile))
  }

  private def generateProfile(seqLst: List[Iterator[Char]]): List[(Long, Long, Long, Long)] = {
    def updateProfile(itrList: List[Iterator[Char]], profile: List[(Long, Long, Long, Long)]): List[(Long, Long, Long, Long)] = {
      def getHeadProfile(cList: List[Char]): (Long, Long, Long, Long) = {
        cList.foldLeft((0L, 0L, 0L, 0L))((acc: (Long, Long, Long, Long), c: Char) => {
          c.toLower match {
            case 'a' => (acc._1 + 1, acc._2, acc._3, acc._4)
            case 'c' => (acc._1, acc._2 + 1, acc._3, acc._4)
            case 'g' => (acc._1, acc._2, acc._3 + 1, acc._4)
            case 't' => (acc._1, acc._2, acc._3, acc._4 + 1)
          }
        })
      }
      if (itrList.exists(_.hasNext != true)) profile
      else {
        val row = for (seq <- itrList) yield seq.next
        val headProfile: (Long, Long, Long, Long) = getHeadProfile(row)
        updateProfile(itrList, headProfile :: profile)
      }
    }
    updateProfile(seqLst, Nil).reverse
  }

  private def consensus(profile: List[(Long, Long, Long, Long)]): List[Char] = {
    def doConsensus(prof: List[(Long, Long, Long, Long)], s: List[Char]): List[Char] = {
      if (prof.isEmpty) s
      else {
        val h = prof.head
        val m = h._1.max(h._2.max(h._3.max(h._4)))
        val c = if (h._1 == m) 'A' else if (h._2 == m) 'C' else if (h._3 == m) 'G' else 'T'
        doConsensus(prof.tail, c :: s)
      }
    }
    doConsensus(profile, List()).reverse
  }

  /**
   * Display a profile matrix on the console.
   */
  def printProfile(profile: List[(Long, Long, Long, Long)]): Unit = {
    def printNextRow(p: List[(Long, Long, Long, Long)], nt: Int): Unit = {
      if (!p.isEmpty) {
        val r: List[Long] = for (rc <- p) yield {
          nt match {
            case 1 => rc._1
            case 2 => rc._2
            case 3 => rc._3
            case 4 => rc._4
          }
        }
        nt match {
          case 1 => print("A: ")
          case 2 => print("C: ")
          case 3 => print("G: ")
          case 4 => print("T: ")
        }
        println(r.mkString(" "))
      }
    }
    printNextRow(profile, 1)
    printNextRow(profile, 2)
    printNextRow(profile, 3)
    printNextRow(profile, 4)
  }
}

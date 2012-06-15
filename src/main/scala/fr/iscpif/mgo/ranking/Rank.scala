/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo._

object Rank {
  
  def firstRanked[I <: Individual[_] with Rank](individuals: IndexedSeq[I]): IndexedSeq[I] = {
    if(individuals.isEmpty) individuals
    else {
      val ranks = individuals.map{_.rank}
      val firstRank = ranks.min
      individuals filter { i => i.rank == firstRank }
    }
  }
  
}

trait Rank {
  def rank: Int
}
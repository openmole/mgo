/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo._

object Rank {
  
  def firstRanked[G, MF <: Rank](individuals: Population[G, MF]): IndexedSeq[PopulationElement[G, MF]] = {
    if(individuals.isEmpty) individuals
    else {
      val ranks = individuals.map{_.metaFitness.rank}
      val firstRank = ranks.min
      individuals filter { i => i.metaFitness.rank == firstRank }
    }
  }
  
}

trait Rank {
  def rank: Int
}
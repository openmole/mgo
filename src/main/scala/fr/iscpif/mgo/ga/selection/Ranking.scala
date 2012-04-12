/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.selection

import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.tools.Math
import fr.iscpif.mgo.ga.GAGenome
import fr.iscpif.mgo.ga.domination._

object Ranking {

  def sameFirstRanked[I <: Individual[GAGenome, GAFitness] with Ranking](
    a1: IndexedSeq[I],
    a2: IndexedSeq[I]) =
    Math.allTheSame (
      firstRanked(a1).map{_.fitness.values},
      firstRanked(a2).map{_.fitness.values}
    )
  
  
  /*def firstRanked[I <: Individual[_, _] with Ranking](individuals: IndexedSeq[I]): IndexedSeq[I] =
    if(individuals.isEmpty) individuals
  else {
    val first = individuals.map{_.rank}.min
    individuals.filter(_.rank == first)
  }*/
    
  def firstRanked[I <: Individual[GAGenome, GAFitness] with Ranking](individuals: IndexedSeq[I]): IndexedSeq[I] = {
    if(individuals.isEmpty) individuals
    else {
      val ranks = individuals.map{_.rank}
      val firstRank = ranks.min
      individuals filter { i => i.rank == firstRank }
    }
  }
}

trait Ranking {
  def rank: Int
}
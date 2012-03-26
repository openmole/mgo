/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.selection

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection._
import fr.iscpif.mgo.tools.Math

object Ranking {
  
  def samePareto[I <: Individual[_, GAFitness] with Ranking](
    a1: IndexedSeq[I],
    a2: IndexedSeq[I]) = 
      Math.allTheSameSorted(pareto(a1).map{_.fitness.fitness}, pareto(a2).map{_.fitness.fitness})
  
  
  def pareto[I <: Individual[_, _] with Ranking](individuals: IndexedSeq[I]) = 
    if(individuals.isEmpty) individuals
    else {
      val first = individuals.map{_.rank}.min
      individuals.filter(_.rank == first)
    }
  
  def rank[F <: GAFitness](individuals: IndexedSeq [Individual[_, F]], dominanceType: Dominant): IndexedSeq[Ranking] = { 
    import dominanceType._
    
    individuals.zipWithIndex.map { 
      case (indiv, index) =>
        new Ranking {
          val rank = 
            individuals.zipWithIndex.filter {
              case (_, index2) => index != index2
            }.count { 
              case(indiv2, _) => indiv.isDominated(indiv2)
            }
        }
    }
  }
}


trait Ranking {
  def rank: Int
}

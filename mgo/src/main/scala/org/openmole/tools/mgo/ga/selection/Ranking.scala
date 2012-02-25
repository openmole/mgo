/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.selection

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga.GAFitness
import org.openmole.tools.mgo.ga.domination._
import org.openmole.tools.mgo.ga.selection._
import org.openmole.tools.mgo.tools.Math

object Ranking {
  
  def samePareto[I <: Individual[_, GAFitness] with Ranking](
    a1: IndexedSeq[I],
    a2: IndexedSeq[I]) = 
      !Math.allTheSameSorted(pareto(a1).map{_.fitness.fitness}, pareto(a2).map{_.fitness.fitness})
  
  
  def pareto[I <: Individual[_, _] with Ranking](individuals: IndexedSeq[I]) = {
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

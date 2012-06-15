/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo.selection._

object ParetoRanking {
  
  def apply[G <: Genome](individuals: IndexedSeq [Individual[G]], dominance: Dominance): IndexedSeq[Rank] = { 
    individuals.zipWithIndex.map { 
      case (indiv, index) =>
        new Rank {
          lazy val rank = 
            individuals.zipWithIndex.filter {
              case (_, index2) => index != index2
            }.count { 
              case(indiv2, _) => dominance.isDominated(indiv.fitness, indiv2.fitness)
            }
        }
    }
  }
  
  
}


trait ParetoRanking extends Ranking { this: Evolution with MG =>
    
  def rank(individuals: IndexedSeq [Individual[G]]): IndexedSeq[Rank] = 
    ParetoRanking(individuals, this)
  
}




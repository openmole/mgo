/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo.selection._

trait ParetoRanking extends Ranking { this: Evolution with MG =>
    
  def rank(individuals: IndexedSeq [Individual[_, FIT]]): IndexedSeq[Rank] = { 
    individuals.zipWithIndex.map { 
      case (indiv, index) =>
        new Rank {
          val rank = 
            individuals.zipWithIndex.filter {
              case (_, index2) => index != index2
            }.count { 
              case(indiv2, _) => isDominated(indiv.fitness, indiv2.fitness)
            }
        }
    }
  }
}




/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo.selection._

object ParetoRanking {
  
  def apply[G](evaluated: IndexedSeq[(G, Fitness)], dominance: Dominance): IndexedSeq[Int] = { 
    evaluated.zipWithIndex.map { 
      case (indiv, index) =>
        evaluated.zipWithIndex.filter {
          case (_, index2) => index != index2
        }.count { 
          case(indiv2, _) => dominance.isDominated(indiv._2, indiv2._2)
        }
    }
  }
  
  
}


trait ParetoRanking extends Ranking { this: Evolution with MG =>
    
  def rank(evaluated: IndexedSeq[(G, Fitness)]): IndexedSeq[Int] = ParetoRanking(evaluated, this)
  
}




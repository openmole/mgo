/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.selection

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.GAGenome
import fr.iscpif.mgo.ga.domination._
import fr.iscpif.mgo.ga.selection._

class RankPareto extends Rank {
    
  override def apply(individuals: IndexedSeq [Individual[GAGenome, GAFitness]], dominanceType: Dominant): IndexedSeq[Ranking] = { 
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




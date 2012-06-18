/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.ranking._

trait RankDiversityModifier extends Modifier { this: Evolution with Ranking with DiversityMetric =>

  override type MF = Diversity with Rank
  
  override def toPopulation(evaluated: IndexedSeq[Individual[G]]) = {  
    val ranks = rank(evaluated)
    val distances = diversity(evaluated)
      
    (evaluated zip ranks zip distances) map {
      case ((i, r), d) => 
        PopulationElement(
            i,
            new Diversity with Rank {
              val diversity = d
              val rank = r
            }
        )
    }
    
  }
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.ranking._

trait RankDiversityModifier extends Modifier { this: Evolution with Ranking with DiversityMetric =>

  override type I = Individual[G] with Diversity with Rank
  
  override def toPopulation(evaluated: IndexedSeq[(G, Fitness)]) = {  
    val ranks = rank(evaluated)
    val distances = diversity(evaluated)
      
    (evaluated zip ranks zip distances) map {
      case (((g, f), r), d) => 
        PopulationElement(
            g, 
            f,
            new Individual[G] with Diversity with Rank {
              val genome = g
              val fitness = f
              val diversity = d
              val rank = r
            }
        )
    }
    
  }
}

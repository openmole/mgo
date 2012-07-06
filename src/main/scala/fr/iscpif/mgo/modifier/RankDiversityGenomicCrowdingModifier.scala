/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.ga._

trait RankDiversityGenomicCrowdingModifier extends Modifier { this: Evolution with Ranking with DiversityMetric {type G <: GAGenome} =>

  override type MF = Diversity with Rank
  
  override def toPopulation(evaluated: IndexedSeq[Individual[G]]) = {  
    val distances = diversity(evaluated)
    val genomeDiversity = CrowdingDistance(evaluated.map{_.genome.values})
    
    val diversityFitnesses = 
      (evaluated zip genomeDiversity).map{ 
        case(i, gd) => Fitness(i.fitness.values.toList ::: 1 / gd :: Nil)
      }
    
    val ranks = 
      rank( (evaluated zip diversityFitnesses).map{ case(i, fd) =>  Individual(i.genome, fd) } )
    
    (evaluated zip ranks zip distances) map {
      case ((i, r), d) => 
        PopulationElement(i,
            new Diversity with Rank {
              val diversity = d
              val rank = r
            }
        )
    }
    
  }
  
  
}

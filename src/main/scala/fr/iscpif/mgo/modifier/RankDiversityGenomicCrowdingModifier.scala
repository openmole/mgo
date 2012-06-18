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

  override type I = Individual[G] with Diversity with Rank
  
  override def toPopulation(evaluated: IndexedSeq[(G, Fitness)]) = {  
    val distances = diversity(evaluated)
    val genomeDiversity = CrowdingDistance(evaluated.map{_._1.values})
    val diversityFitnesses = 
      (evaluated zip genomeDiversity).map{ case((_, f), gd) => Fitness(f.values.toList ::: 1 / gd :: Nil)}
    val ranks = 
      rank( (evaluated zip diversityFitnesses).map{ case((g, _), fd) =>  g -> fd } )
    
    (evaluated zip ranks zip distances zip diversityFitnesses) map {
      case ((((g, f), r), d), fd) => 
        PopulationElement(g, f,
            new Individual[G] with Diversity with Rank {
              val genome = g
              val fitness = fd
              
              val diversity = d
              val rank = r
            }
        )
    }
    
  }
  
  
}

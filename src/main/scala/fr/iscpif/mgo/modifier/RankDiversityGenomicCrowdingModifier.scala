/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import metric._

trait RankDiversityGenomicCrowdingModifier extends Modifier with Ranking with DiversityMetric {
  self: G { type G <: GAGenome } => 
  
  override type MF = Diversity with Rank
  
  override def toPopulation(evaluated: IndexedSeq[Individual[G]]) = {  
    val genomeDiversity = CrowdingDistance(evaluated.map{_.genome.values})
    
    val diversityFitnesses = 
      (evaluated zip genomeDiversity).map{ 
        case(i, gd) => Fitness(i.fitness.values.toList ::: 1 / gd() :: Nil)
      }
    
    val ranks = 
      rank( (evaluated zip diversityFitnesses).map{ case(i, fd) =>  Individual(i.genome, fd) } )
    
    
    val distances = diversity(evaluated zip ranks)
    
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

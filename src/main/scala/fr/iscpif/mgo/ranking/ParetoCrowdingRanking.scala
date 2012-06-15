/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import fr.iscpif.mgo.diversity.CrowdingDistance
import fr.iscpif.mgo.ga._

trait ParetoCrowdingRanking extends ParetoRanking { 
  this: Evolution with MG { type G <: GAGenome} =>

  override def rank(individuals: IndexedSeq [Individual[G]]): IndexedSeq[Rank] = { 
    val crowding = CrowdingDistance(individuals.map{_.genome.values})
    
    super.rank(
      (individuals zip crowding).map {
        case(i, c) =>
        new Individual[G] {
          def genome = i.genome
          def fitness = new Fitness {
            val values = (i.fitness.values.toList ::: 1 / c :: Nil).toIndexedSeq
          }
        }
      }
    )
  }
}

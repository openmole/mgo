/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo.Individual
/*
class ParetoCrowdingRank extends ParetoRank {
  
  override def apply(individuals: IndexedSeq [Individual[GAGenome, GAFitness]], dominanceType: Dominant): IndexedSeq[Ranking] = {
    val crowding = Distance.crowding(individuals)
    
    super.apply(
      individuals.zip(crowding).map {
        case(i, c) =>
        new Individual[GAGenome, GAFitness] {
          def genome = i.genome
          def fitness = new GAFitness {
            val values = (1 / c.distance :: i.fitness.values.toList).toIndexedSeq
          }
        }
      }, dominanceType)
  }
}
*/
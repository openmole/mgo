/*
 * Copyright (C) 2011 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package org.openmole.tools.mgo.asrea

import org.openmole.tools.mgo.evolution.EvolutionEngine
import org.openmole.tools.mgo.model.Individual
import org.openmole.tools.mgo.model.MultiGoalLike
import org.openmole.tools.mgo.model.Population
import org.openmole.tools.mgo.model.Population._
import scala.collection.mutable.ArrayBuffer
import org.openmole.tools.mgo.domination.diversity.Crowding._
import scala.collection.mutable.ListBuffer
import java.util.Random


class ASREA[GE](var population: Population[Individual[GE,_]], archive: Archive[Individual[GE,_]], engine: EvolutionEngine[GE], fitness: GE => MultiGoalLike)(implicit rng: Random) {
  
  def apply(nbGeneration: Int) = {
    
    for(gen <- 0 until nbGeneration) {
      
      val childPopulation = new Population(engine.apply(toGenomes(population), population.size * 2).map{ g => new Individual(g, fitness(g))})
      childPopulation.individuals.foreach{ archive += _ }
      val childPopulationAndCrowding = orderByDecreasingCrowding(childPopulation.individuals)
      //val rankOfIndiv = childPopulation.individuals.map{ i => (i, archive.ranks(i)) }
      val newPopulation = new ListBuffer[Individual[GE,_]]
      
      val archiveAndCrowding = orderByDecreasingCrowding(archive.individuals)
      
      for(i <- 0 until population.size) {
        val i1 = archiveAndCrowding(rng.nextInt(archive.size))
        val i2 = archiveAndCrowding(rng.nextInt(archive.size))
        if(i1._2 != i2._2) newPopulation += List(i1, i2).sortWith( (x,y) => x._2 < y._2 ).head._1
        else newPopulation += IndexedSeq(i1, i2)(rng.nextInt(2))._1
      }
      
      for(i <- 0 until population.size) {
        val i1 = childPopulationAndCrowding(rng.nextInt(childPopulation.individuals.size))
        val i2 = childPopulationAndCrowding(rng.nextInt(childPopulation.individuals.size))
        val rankI1 = archive.ranks(i1._1)
        val rankI2 = archive.ranks(i2._1)
        
        if(rankI1 != rankI2) newPopulation += {if(rankI1 < rankI2) i1._1 else i2._1}
        else newPopulation += IndexedSeq(i1, i2).apply(rng.nextInt(2))._1
      }
      
      population = new Population(newPopulation.toIndexedSeq)
    }
  }
  
}

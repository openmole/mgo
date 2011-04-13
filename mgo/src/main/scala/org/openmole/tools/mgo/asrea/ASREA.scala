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
import org.openmole.tools.mgo.model.Population._
import scala.collection.mutable.ArrayBuffer
import org.openmole.tools.mgo.domination.diversity.Crowding._
import scala.collection.mutable.ListBuffer
import java.util.Random


class ASREA[GE](engine: EvolutionEngine[GE], fitness: GE => MultiGoalLike) {

  def evolve(initialPopulation: Iterable[Individual[GE,_]], archiveContent: Iterable[Individual[GE,_]], nbGeneration: Int) (implicit rng: Random) = {
    
    val archive = new Archive(archiveContent)
    var population = initialPopulation.toList
    
    for(gen <- 0 until nbGeneration) {
      val childPopulation = engine(population.toGenomes.toIndexedSeq, population.size * 2).map{g => new Individual(g, fitness(g))}
      childPopulation.foreach{ archive += _ }
      val childPopulationAndCrowding = childPopulation.orderByDecreasingCrowding
      //val rankOfIndiv = childPopulation.individuals.map{ i => (i, archive.ranks(i)) }
      population = Nil  
      val archiveAndCrowding = archive.orderByDecreasingCrowding
      
      for(i <- 0 until population.size) {
        val i1 = archiveAndCrowding(rng.nextInt(archive.size))
        val i2 = archiveAndCrowding(rng.nextInt(archive.size))
        if(i1._2 != i2._2) population ::= List(i1, i2).sortWith( (x,y) => x._2 < y._2 ).head._1
        else population ::= IndexedSeq(i1, i2)(rng.nextInt(2))._1
      }
      
      for(i <- 0 until population.size) {
        val i1 = childPopulationAndCrowding(rng.nextInt(childPopulation.size))
        val i2 = childPopulationAndCrowding(rng.nextInt(childPopulation.size))
        val rankI1 = archive.ranks(i1._1)
        val rankI2 = archive.ranks(i2._1)
        
        if(rankI1 != rankI2) population ::= {if(rankI1 < rankI2) i1._1 else i2._1}
        else population ::= IndexedSeq(i1, i2).apply(rng.nextInt(2))._1
      }
    }
    (population, archive)
  }
  
}

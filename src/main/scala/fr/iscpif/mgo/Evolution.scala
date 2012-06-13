 /*
 * Copyright (C) 2011 srey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo

import annotation.tailrec
import fr.iscpif.mgo.crossover.CrossOver
import fr.iscpif.mgo._
import fr.iscpif.mgo.mutation.Mutation
import fr.iscpif.mgo.selection.Selection
import fr.iscpif.mgo.termination.Termination
import java.util.Random
import Individual._

trait Evolution extends Mutation with CrossOver with Termination with Selection { self =>

  type I 
  type G <: Genome
  type F <: GenomeFactory[G]
  type FIT <: Fitness
  type RAWFIT <: Fitness
  
  def factory: F

  //Perform N step
  @tailrec private def evolveStep(population: IndexedSeq[I], evaluator: G => FIT, state: STATE = initialState)(implicit aprng:Random):IndexedSeq[I]= {
    val nextPop = evolve(population, evaluator)
    val (end, newState) = terminated(population, nextPop, state)
    if (end) nextPop
    else evolveStep(nextPop, evaluator, newState)
  }
  
  def run(population: IndexedSeq[I], evaluator: G => FIT) (implicit aprng: Random): IndexedSeq[I] = evolveStep(population, evaluator)
  def run(populationSize: Int, evaluator: G => FIT)(implicit aprng: Random): IndexedSeq[I] = evolveStep(randomPopulation(populationSize, evaluator), evaluator)
  
  def evolve(population: IndexedSeq[I], evaluator: G => FIT)(implicit aprng: Random): IndexedSeq[I]
  
  def toI(individuals: IndexedSeq[Individual[G, FIT]]): IndexedSeq[I]
  
  def randomPopulation(size: Int, evaluator: G => FIT)(implicit aprng: Random): IndexedSeq[I] =
    toI((0 until size).map{ 
        i => Individual(factory.random, evaluator)
      }).toIndexedSeq
  
}

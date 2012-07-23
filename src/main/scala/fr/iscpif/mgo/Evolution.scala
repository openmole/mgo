/*
 * Copyright (C) 2011 Sebastien Rey
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

import Individual._
import java.util.Random

trait Evolution extends Mutation with CrossOver with Termination with Selection with Modifier { self =>

  type G <: Genome
  type MF 

  implicit val factory: Factory[G]

  def lambda: Int
  
  def run(population: Population[G, MF], evaluator: G => Fitness)(implicit aprng: Random): Iterator[(Population[G, MF], STATE, Boolean)] = 
    Iterator.iterate((population, initialState(population), false)){
      case(population, state, _) => 
        val newPop = evolve(population, evaluator) 
        val (stop, newState) = terminated(population, state)
        (newPop, newState, stop)                        
    }
  
  
  def run(evaluator: G => Fitness)(implicit aprng: Random): Iterator[(Population[G, MF], STATE, Boolean)] = 
    run(randomPopulation(evaluator), evaluator)
  
  def run[P <: Problem {type G >: self.G}](problem: P)(implicit aprng: Random): Iterator[(Population[G, MF], STATE, Boolean)] = 
    run(problem.apply _)
  
  def evolve(population: Population[G, MF], evaluator: G => Fitness)(implicit aprng: Random): Population[G, MF]
  
  def randomPopulation(evaluator: G => Fitness)(implicit aprng: Random): Population[G, MF] =
    toPopulation((0 until lambda).map{ _ => factory.random }.par.map{ g => Individual(g, evaluator)}.toIndexedSeq)
  
  def emptyPopulation: Population[G, MF] = toPopulation(IndexedSeq.empty)
  
  def stepListner(population: Population[G, MF], state: STATE) = {}
  
}

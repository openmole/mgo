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

/**
 * Trait evolution provide the feature to define an evolutionary algorithm
 */
trait Evolution extends Termination
     with Modifier 
     with Lambda 
     with G
     with F
     with MF 
     with GenomeFactory
     with Archive { self =>


  /** 
   * Represent a state of the evolution algorithm
   */
  case class EvolutionState(
    /** The current population of solution */
    val population: Population[G, F, MF],
    /** The current achive */
    val archive: A,
    /** The number of the generation */
    val generation: Int,
    /** The state maintained for the termination criterium */
    val terminationState: STATE,
    /** true if the termination criterium is met false otherwhise */
    val terminated: Boolean
  )
 
  /**
   * Run the evolutionary algorithm
   * 
   * @param population the initial population
   * @param evaluator the fitness evaluation function
   * @return an iterator over the states of the evolution
   */
  def run(p: Population[G, F, MF], a: A, evaluator: G => F)(implicit aprng: Random): Iterator[EvolutionState] =
    Iterator.iterate(EvolutionState(p, a, 0, initialState(p), false)){
      s => 
        val newPop = evolve(s.population, s.archive, evaluator)
        val newArch = archive(s.archive, newPop)
        val (stop, newState) = terminated(newPop, s.terminationState)
        EvolutionState(newPop, newArch, s.generation + 1, newState, stop)
    }
  
  /** 
   * Run the evolutionary algorithm
   * 
   * @param evaluator the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def run(evaluator: G => F)(implicit aprng: Random): Iterator[EvolutionState] = {
    val archive = initialArchive
    run(randomPopulation(evaluator, archive), archive, evaluator)
  }
  
  /**
   * Run the evlutionary algorithm
   * 
   * @param problem an optimization problem to solve
   */
  def run[P <: Problem {type G >: self.G; type F <: self.F}](problem: P)(implicit aprng: Random): Iterator[EvolutionState] =
    run(s => problem.apply(s))
  
  /**
   * Evolve one step
   * 
   * @param population the current population
   * @param evaluator the fitness evaluation function
   * @return a new population of evaluated solutions
   * 
   */
  def evolve(population: Population[G, F, MF], archive: A, evaluator: G => F)(implicit aprng: Random): Population[G, F, MF]
  
  /**
   * Generate an random population
   * 
   * @param evaluator the fitness evaluation function
   * @return a random population of evaluated solutions
   */
  def randomPopulation(evaluator: G => F, archive: A)(implicit aprng: Random): Population[G, F, MF] =
    toPopulation(
      (0 until lambda).map{ _ => genomeFactory.random }.par.map{ g => Individual[G, F](g, evaluator)}.toIndexedSeq,
      archive
    )
  
}

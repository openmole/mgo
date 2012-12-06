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
    with Archive
    with Breeding
    with Elitism { self =>

  /**
   * Represent a state of the evolution algorithm
   */
  case class EvolutionState(
    /** The current population of solution */
    val individuals: Seq[Individual[G, F]],
    /** The current achive */
    val archive: A,
    /** The number of the generation */
    val generation: Int,
    /** The state maintained for the termination criterium */
    val terminationState: STATE,
    /** true if the termination criterium is met false otherwhise */
    val terminated: Boolean)

  /**
   * Run the evolutionary algorithm
   *
   * @param population the initial population
   * @param evaluator the fitness evaluation function
   * @return an iterator over the states of the evolution
   */
  def run(p: Seq[Individual[G, F]], a: A, evaluator: G => F)(implicit aprng: Random): Iterator[EvolutionState] =
    Iterator.iterate(EvolutionState(p, a, 0, initialState, false)) {
      s =>
        val (newPop, newArchive) = evolve(s.individuals, s.archive, evaluator)
        val (stop, newState) = terminated(toPopulation(newPop, newArchive), s.terminationState)
        EvolutionState(newPop, newArchive, s.generation + 1, newState, stop)
    }

  /**
   * Run the evolutionary algorithm
   *
   * @param evaluator the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def run(evaluator: G => F)(implicit aprng: Random): Iterator[EvolutionState] = {
    val archive = initialArchive
    val individuals = random(evaluator, archive)
    run(individuals, archive, evaluator)
  }

  /**
   * Run the evlutionary algorithm
   *
   * @param problem an optimization problem to solve
   */
  def run[P <: Problem { type G >: self.G; type F <: self.F }](problem: P)(implicit aprng: Random): Iterator[EvolutionState] =
    run(s => problem.apply(s))

  /**
   * Evolve one step
   *
   * @param population the current population
   * @param evaluator the fitness evaluation function
   * @return a new population of evaluated solutions
   *
   */
  def evolve(individuals: Seq[Individual[G, F]], archive: A, evaluator: G => F)(implicit aprng: Random): (Seq[Individual[G, F]], A) = {
    val offspring = breed(
      individuals, archive
    ).par.map { g => Individual(g, evaluator) }.seq

    val newIndividuals = offspring.toList ::: individuals.toList
    val newArchive = combine(archive, toArchive(offspring))

    //Elitism strategy
    (elitism(newIndividuals, newArchive), newArchive)
  }

  /**
   * Generate an random population
   *
   * @param evaluator the fitness evaluation function
   * @return a random population of evaluated solutions
   */
  def random(evaluator: G => F, archive: A)(implicit aprng: Random): Seq[Individual[G, F]] =
    breed(Seq.empty, archive).par.map { g => Individual[G, F](g, evaluator) }.seq

}

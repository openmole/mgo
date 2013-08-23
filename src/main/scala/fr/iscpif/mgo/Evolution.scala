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
import util.Random
import org.apache.commons.math3.random.{ RandomAdaptor, Well44497b }

/**
 * Trait evolution provide the feature to define an evolutionary algorithm
 */
trait Evolution extends Termination
    with Modifier
    with Lambda
    with G
    with F
    with P
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
    individuals: Seq[Individual[G, P, F]],
    /** The current achive */
    archive: A,
    /** The number of the generation */
    generation: Int,
    /** The state maintained for the termination criterium */
    terminationState: STATE,
    /** true if the termination criterium is met false otherwhise */
    terminated: Boolean)

  def buildRNG(seed: Long) = new Random(new RandomAdaptor(new Well44497b(seed)))

  /**
   * Run the evolutionary algorithm
   *
   * @param population the initial population
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(population: Seq[Individual[G, P, F]], a: A, expression: G => P, evaluation: (P, Random) => F)(implicit aprng: Random): Iterator[EvolutionState] =
    Iterator.iterate(EvolutionState(population, a, 0, initialState, false)) {
      s =>
        val (newPop, newArchive) = step(s.individuals, s.archive, s.generation, expression, evaluation)
        val (stop, newState) = terminated(toPopulation(newPop, newArchive), s.terminationState)
        EvolutionState(newPop, newArchive, s.generation + 1, newState, stop)
    }

  /**
   * Run the evolutionary algorithm
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(expression: G => P, evaluation: (P, Random) => F)(implicit prng: Random): Iterator[EvolutionState] = {
    val archive = initialArchive
    evolve(Seq.empty, archive, expression, evaluation)
  }

  /**
   * Evolve one step
   *
   * @param individuals the current population
   * @param archive the current archive
   * @param expression expression of the genome
   * @param evaluation the fitness evaluator
   * @return a new population of evaluated solutions
   *
   */
  def step(individuals: Seq[Individual[G, P, F]], archive: A, generation: Int, expression: G => P, evaluation: (P, Random) => F)(implicit rng: Random): (Seq[Individual[G, P, F]], A) = {
    val offspringGenomes = breed(individuals, archive, generation)
    val rngs = (0 until offspringGenomes.size).map(_ => buildRNG(rng.nextLong))

    val offspring = (offspringGenomes zip rngs).par.map { case (g, rng) => Individual[G, P, F](g, expression, evaluation)(rng) }.seq

    val newIndividuals = offspring.toList ::: individuals.toList
    val newArchive = combine(archive, toArchive(offspring))

    //Elitism strategy
    (elitism(newIndividuals, newArchive), newArchive)
  }

}

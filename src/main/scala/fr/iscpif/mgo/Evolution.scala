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

import fr.iscpif.mgo.Population

import util.Random
import org.apache.commons.math3.random.{ RandomAdaptor, Well44497b }

import fr.iscpif.mgo.tools.time

/**
 * Trait evolution provide the feature to define an evolutionary algorithm
 */
trait Evolution extends Termination
    with Lambda
    with G
    with F
    with P
    with Archive
    with Breeding
    with Elitism { self =>

  /**
   * Represent a state of the evolution algorithm
   */
  case class EvolutionState(
      /** The current population of solution */
      population: Population[G, P, F],
      /** The current archive */
      archive: A,
      /** The number of the generation */
      generation: Int,
      /** The state maintained for the termination criterium */
      terminationState: STATE,
      /** true if the termination criterium is met false otherwhise */
      terminated: Boolean) {
  }

  def buildRNG(seed: Long) = new Random(new RandomAdaptor(new Well44497b(seed)))

  /**
   * Run the evolutionary algorithm
   *
   * @param population the initial individuals
   * @param archive the initial archive
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(population: Population[G, P, F], archive: A, expression: (G, Random) => P, evaluation: (P, Random) => F)(implicit rng: Random): Iterator[EvolutionState] =
    Iterator.iterate(EvolutionState(population, archive, 0, initialState, false)) {
      s =>
        val (newIndividuals, newArchive) = step(s.population, s.archive, expression, evaluation)
        val newPop = Population(newIndividuals)
        val (stop, newState) = terminated(newPop, s.terminationState)
        EvolutionState(newPop, newArchive, s.generation + 1, newState, stop)
    }

  /**
   * Run the evolutionary algorithm
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(expression: (G, Random) => P, evaluation: (P, Random) => F)(implicit prng: Random): Iterator[EvolutionState] = {
    val archive = initialArchive
    evolve(Seq.empty, archive, expression, evaluation)
  }

  /**
   * Run the evolutionary algorithm
   * @param population the initial individuals
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(population: Population[G, P, F], expression: (G, Random) => P, evaluation: (P, Random) => F)(implicit prng: Random): Iterator[EvolutionState] = {
    val archive = initialArchive
    evolve(population, archive, expression, evaluation)
  }

  /**
   * Run the evolutionary algorithm
   * @param population the initial individuals
   * @param offspring the initial offsprings
   * @param expression the genome expression
   * @param evaluation the fitness evaluator
   * @return an iterator over the states of the evolution
   */
  def evolve(population: Population[G, P, F], offspring: Population[G, P, F], expression: (G, Random) => P, evaluation: (P, Random) => F)(implicit prng: Random): Iterator[EvolutionState] = {
    val archive = self.archive(initialArchive, population, offspring)
    evolve(population, archive, expression, evaluation)
  }
  /**
   * Evolve one step
   *
   * @param population the current population
   * @param archive the current archive
   * @param expression expression of the genome
   * @param evaluation the fitness evaluator
   * @return a new population of evaluated solutions
   *
   */
  def step(population: Population[G, P, F], archive: A, expression: (G, Random) => P, evaluation: (P, Random) => F)(implicit rng: Random): (Population[G, P, F], A) = {
    val offspringGenomes = breed(population, archive, lambda)
    val rngs = (0 until offspringGenomes.size).map(_ => buildRNG(rng.nextLong))

    val offspring = Population.fromIndividuals((offspringGenomes zip rngs).par.map { case (g, rng) => Individual[G, P, F](g, expression, evaluation)(rng) }.seq)

    val newArchive = self.archive(archive, population, offspring)

    //Elitism strategy
    (elitism(population, offspring.toList, newArchive), newArchive)
  }

}

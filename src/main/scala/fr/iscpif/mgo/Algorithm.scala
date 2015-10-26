/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.mgo

import monocle.macros._

import scala.util.Random
import scalaz._
import Scalaz._

trait Pop {
  type G
  type P
  type Pop = Population[G, P]
  type Ind = Individual[G, P]
}

trait Algorithm extends Pop {

  /** Type of the state maintained to study the evolution of the algorithm */
  type STATE

  /**
    * Represent a state of the evolution algorithm
    */
  @Lenses case class EvolutionState(
    state: STATE,
    generation: Int = 0,
    random: Random)

  def updateGeneration = State[EvolutionState, Unit] { s => EvolutionState.generation.modify(_ + 1)(s) }

  def mu: Int
  def randomGenome: State[Random, G]

  def breeding(population: Pop): State[EvolutionState, Vector[G]]
  def elitism(population: Pop, offspring: Pop): State[EvolutionState, Pop]
  def termination: State[EvolutionState, Boolean]

  def step(population: Pop, express: (G => State[Random, P])): State[EvolutionState, Pop] = {
    def expressMonad(g: G) = State { state: EvolutionState => (state, Individual[G, P](g, express).eval(state.random)) }

    def randomIfEmpty =
      if(population.content.isEmpty) EvolutionState.random.lifts(randomGenome).generate(mu).map(_.toVector)
      else breeding(population)

    for {
      breed <- randomIfEmpty
      offspring <- breed.traverseS { expressMonad }
      population <- elitism(population, offspring)
      _ <- updateGeneration
    } yield population
  }

}

trait AlgorithmDefault <: Algorithm with ElitismDefault with Genome


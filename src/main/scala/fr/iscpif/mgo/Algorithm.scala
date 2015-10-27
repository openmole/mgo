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
  @Lenses case class AlgorithmState(
    state: STATE,
    generation: Int = 0,
    random: Random)

  def mu: Int
  def lambda: Int

  def initialState: STATE
  def algorithmState(random: Random) = AlgorithmState(state = initialState, random = random)

  def updateGeneration = State[AlgorithmState, Unit] { s => AlgorithmState.generation.modify(_ + 1)(s) }

  def breeding(population: Pop): State[AlgorithmState, Vector[G]]
  def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop]

}



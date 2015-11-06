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

import Algorithm.CommonState
import monocle.macros._

import scala.util.Random
import scalaz._
import Scalaz._

trait Pop {
  type G
  type P
  type Ind = Individual[G, P]
  type Pop = Population[Ind]
}

object Algorithm {
  /**
   * Represent a state of the evolution algorithm
   */
  @Lenses case class CommonState(
    generation: Long @@ Generation,
    startTime: Long @@ Start = System.currentTimeMillis(),
    random: Random)
}

trait Algorithm extends Pop {

  /** Type of the state maintained to study the evolution of the algorithm */
  type STATE

  case class AlgorithmState(
    common: CommonState,
    state: STATE)

  implicit def common = monocle.macros.Lenser[AlgorithmState](_.common)
  implicit def state = monocle.macros.Lenser[AlgorithmState](_.state)
  implicit def generation = common composeLens CommonState.generation
  implicit def startTime = common composeLens CommonState.startTime
  implicit def random = common composeLens CommonState.random

  def initialState: STATE

  def algorithmState(random: Random, generation: Long = 0) =
    AlgorithmState(state = initialState, common = CommonState(random = random, generation = generation))

  def updateGeneration = State[AlgorithmState, Unit] { s => generation.modify(_ + 1)(s) }

  def breeding(population: Pop, lambda: Int): State[AlgorithmState, Vector[G]]
  def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop]

}


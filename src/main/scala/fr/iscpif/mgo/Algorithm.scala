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

/**
 * Represent a state of the evolution algorithm
 */
@Lenses case class CommonState(
  generation: Long @@ Generation,
  startTime: Long @@ Start = System.currentTimeMillis(),
  random: Random)

case class AlgorithmState[S](
  common: CommonState,
  state: S)

trait Algorithm[G, P, S] {

  type Ind = Individual[G, P]
  type Pop = Population[Ind]

  def initialState: S

  def algorithmState(random: Random, generation: Long = 0) =
    AlgorithmState(state = initialState, common = CommonState(random = random, generation = generation))

  def breeding(population: Pop, lambda: Int): State[AlgorithmState[S], Vector[G]]
  def elitism(population: Pop, offspring: Pop): State[AlgorithmState[S], Pop]

}


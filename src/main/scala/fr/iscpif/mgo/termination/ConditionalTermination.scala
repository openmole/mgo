/*
 * Copyright (C) 2015 Guillaume Chérel
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

package fr.iscpif.mgo.termination

import fr.iscpif.mgo._
import fr.iscpif.mgo.Population
import fr.iscpif.mgo.genome

import scala.util.Random

/**
 * Layer to compute the stopping condition of the evolutionary algorithm
 */
trait ConditionalTermination extends Termination {

  /** Type of the state maintained to study the evolution of the algorithm */
  type STATE = Boolean

  /**
   * Compute the initial state
   *
   * @return the initial state
   */
  def initialState = false

  /**
   * Test if the algorithm has converged.
   *
   * @param population the current population
   * @param terminationState the actual termination state
   * @return a boolean which is equal to true if a terminal state has
   * been detected and the new termination state
   */
  def terminated(population: Population[G, P, F], terminationState: STATE)(implicit rng: Random): (Boolean, STATE) = {
    val res = terminated(population)
    (res, res)
  }

  def terminated(population: Population[G, P, F])(implicit rng: Random): Boolean

}

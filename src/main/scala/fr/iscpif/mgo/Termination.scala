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

import scalaz._

trait Termination <: Pop { this: Algorithm =>

  type Termination = State[EvolutionState, Boolean]

}

/**
 * Layer to compute the stopping condition of the evolutionary algorithm
 */
/*trait Termination extends G with P with F {



  /**
   * Test if the algorithm has converged.
   *
   * @param population the current population
   * @param terminationState the actual termination state
   * @return a boolean which is equal to true if a terminal state has
   * been detected and the new termination state
   */
  def terminated(population: Population[G, P, F], terminationState: STATE)(implicit rng: Random): (Boolean, STATE)
}*/

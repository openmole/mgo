/*
 * Copyright (C) 23/08/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo.Individual
import scala.util.Random

trait MergedGenerations <: Elitism {
  def elitism(oldGeneration: Seq[Individual[G, P, F]], newGeneration: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random): Seq[Individual[G, P, F]] =
    elitism(newGeneration.toList ::: oldGeneration.toList, archive)

  /**
   * Reduce the number of elements of the population and return a new one
   *
   * @param individuals the population to shrink
   * @return the shrinked population
   */
  def elitism(individuals: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random): Seq[Individual[G, P, F]]

}

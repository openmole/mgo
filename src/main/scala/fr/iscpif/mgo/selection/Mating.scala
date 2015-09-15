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
package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import scala.util.Random
import scalaz._

import scala.language.higherKinds

trait Mating extends G with P with F with Archive with BreedingContext {

  /**
   * Select an individual among the population.
   *
   * param population the population in which selection occurs
   * @return the selected individual
   */
  def mate(population: Population[G, P, F], archive: A)(implicit rng: Random): Iterator[BreedingContext[Vector[Individual[G, P, F]]]]
}

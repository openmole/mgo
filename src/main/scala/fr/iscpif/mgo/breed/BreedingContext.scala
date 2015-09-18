/*
 * Copyright (C) 05/08/2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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
package fr.iscpif.mgo.breed

import fr.iscpif.mgo._

import scala.util.Random
import scalaz._
import Scalaz._

import scala.language.higherKinds

trait BreedingContext <: G with P with F with A {

  type BreedingContext[A]
  implicit def monadBreedingContext: Monad[BreedingContext]

  /** extract the content from the breeding monad */
  def unwrapBreedingContext[Z](x: BreedingContext[Z], population: Population[G, P, F], archive: A): Z

  //def initialGenome(implicit rng: Random): BreedingContext[G]

  //def cloneInContext(c: Individual[G, P, F]): BreedingContext[G]

}

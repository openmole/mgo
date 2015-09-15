/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import util.Random

import scalaz._
import Scalaz._

import monocle.Lens
import monocle.syntax._

import scala.language.higherKinds

/**
 * Mutation of a genome based on gausian distribution arrount the genome with
 * fixed sigma values.
 */
object GaussianMutation {

  /** sigma values, one for each element in the rest of the genome */
  //  def apply(mutation: Mutation with GA)(sigma: Double): mutation.Mutation = {
  //    import mutation._
  //    (g: G, population: Population[G, P, F], archive: A, rng: Random) => {
  //      val newValues = values.get(g) map {
  //        _ + (rng.nextGaussian * sigma)
  //      }
  //      values.set(newValues)(g)
  //    }
  //  }

  def apply[G, P, F, A, BreedingContext[_]: Monad](sigma: Double)(values: Lens[G, Seq[Double]]): (G, Population[G, P, F], A, Random) => BreedingContext[G] = {
    (g: G, population: Population[G, P, F], archive: A, rng: Random) =>
      {
        val newValues = values.get(g) map {
          _ + (rng.nextGaussian * sigma)
        }
        values.set(newValues)(g).point[BreedingContext]
      }
  }
}

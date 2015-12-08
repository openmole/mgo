/*
 * Copyright (C) 04/12/2015 Guillaume Chérel
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
package fr.iscpif.mgo

import scala.language.higherKinds
import scalaz._
import Scalaz._

object Objectives {

  type Objective[I, M[_]] = Vector[I] => M[Vector[I]]

  /** Returns the mu individuals with the highest ranks. */
  def keepBestByO[I, K: Order, M[_]: Monad](f: Vector[I] => Vector[K], mu: Int): Objective[I, M] =
    (individuals: Vector[I]) =>
      if (individuals.size < mu) individuals.point[M]
      else {
        val scores = f(individuals)
        val sortedBestToWorst = (individuals zip scores).sortBy { _._2 }(implicitly[Order[K]].reverseOrder.toScalaOrdering).map { _._1 }

        sortedBestToWorst.take(mu).point[M]
      }

  /**** Clone strategies ****/
  def applyCloneStrategy[I, G, M[_]: Monad](getGenome: I => G, cloneStrategy: Vector[I] => M[Vector[I]]): Objective[I, M] =
    (individuals: Vector[I]) =>
      for {
        res <- individuals.groupBy(getGenome).valuesIterator.toVector.traverseM[M, I](cloneStrategy)
      } yield res

  def clonesKeepYoungest[C, M[_]: Monad](generation: C => Long): Vector[C] => M[Vector[C]] =
    (clones: Vector[C]) => clones.maxBy(generation).point[Vector].point[M]

}

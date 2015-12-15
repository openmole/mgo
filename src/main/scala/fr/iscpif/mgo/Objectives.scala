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

import scala.math.min

object Objectives {

  type Objective[I, M[_]] = Vector[I] => M[Vector[I]]

  def minimiseO[I, M[_]: Monad, F: Order](f: I => F, mu: Int): Objective[I, M] =
    (individuals: Vector[I]) => individuals.sorted(implicitly[Order[F]].contramap[I](f).toScalaOrdering).take(mu).point[M]

  def maximiseO[I, M[_]: Monad, F: Order](f: I => F, mu: Int): Objective[I, M] =
    (individuals: Vector[I]) => individuals.sorted(implicitly[Order[F]].contramap[I](f).reverseOrder.toScalaOrdering).take(mu).point[M]

  /** Returns the mu individuals with the highest ranks. */
  def keepHighestRankedO[I, K: Order, M[_]: Monad](f: Vector[I] => Vector[K], mu: Int): Objective[I, M] =
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

  def clonesKeepYoungest[I, M[_]: Monad](generation: I => Long): Vector[I] => M[Vector[I]] =
    (clones: Vector[I]) => clones.maxBy(generation).point[Vector].point[M]

  //TODO: L'algo suivant suppose que la partie de l'historique du jeune avant young.age est un duplicat du plus vieux.
  //Pas sur que ça soit toujours le cas: On peut avoir 2 clones qui ont évolué différemment et n'ont pas d'histoire commune.
  //Si c'est le cas, alors on supprime une partie d'historique que l'on devrait garder.
  def clonesMergeHistories[I, P, M[_]](historySize: Int)(implicit ia: Age[I], ih: PhenotypeHistory[I, P], m: Monad[M]): Vector[I] => M[Vector[I]] =
    (clones: Vector[I]) => {
      clones.sortBy { i => -ia.getAge(i) }.reduceLeft { (i1, i2) =>
        val oldAge = ia.getAge(i1)
        val youngAge = ia.getAge(i2)

        def oldH: Vector[P] = ih.getHistory(i1)
        def youngH: Vector[P] = ih.getHistory(i2).takeRight(min(youngAge, Int.MaxValue).toInt)

        ia.setAge(ih.setHistory(i1, (oldH ++ youngH).takeRight(historySize)), oldAge + youngAge)
      }
    }.point[Vector].point[M]
}

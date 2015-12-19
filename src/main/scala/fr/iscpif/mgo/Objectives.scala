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

import Contexts._

import scala.math.min

object Objectives {

  //type Objective[I, M[_]] = Vector[I] => M[Vector[I]]
  type Objective[M[_], I] = Kleisli[M, Vector[I], Vector[I]]

  object Objective {
    def apply[M[_]: Monad, I](f: Vector[I] => M[Vector[I]]): Objective[M, I] = Kleisli(f)
  }

  def minimiseO[M[_]: Monad, I, F: Order](f: I => F, mu: Int): Objective[M, I] =
    Objective((individuals: Vector[I]) => individuals.sorted(implicitly[Order[F]].contramap[I](f).toScalaOrdering).take(mu).point[M])

  def maximiseO[M[_]: Monad, I, F: Order](f: I => F, mu: Int): Objective[M, I] =
    Objective((individuals: Vector[I]) => individuals.sorted(implicitly[Order[F]].contramap[I](f).reverseOrder.toScalaOrdering).take(mu).point[M])

  /** Returns n individuals randomly. */
  def randomO[M[_]: Monad: RandomGen, I](n: Int): Objective[M, I] =
    Objective(
      (individuals: Vector[I]) => {
        val popSize = individuals.size
        for {
          rg <- implicitly[RandomGen[M]].get
        } yield Vector.fill(n)(individuals(rg.nextInt(popSize)))
      }
    )

  /** Returns the mu individuals with the highest ranks. */
  def keepHighestRankedO[M[_]: Monad, I, K: Order](f: Kleisli[M, Vector[I], Vector[K]], mu: Int): Objective[M, I] =
    Objective((individuals: Vector[I]) =>
      if (individuals.size < mu) individuals.point[M]
      else
        for {
          scores <- f.run(individuals)
        } yield {
          val sortedBestToWorst = (individuals zip scores).sortBy { _._2 }(implicitly[Order[K]].reverseOrder.toScalaOrdering).map { _._1 }
          sortedBestToWorst.take(mu)
        }
    )

  /**** Clone strategies ****/
  def applyCloneStrategy[M[_]: Monad, I, G](getGenome: I => G, cloneStrategy: Objective[M, I]): Objective[M, I] =
    Objective((individuals: Vector[I]) =>
      for {
        res <- individuals.groupBy(getGenome).valuesIterator.toVector.traverseM[M, I](cloneStrategy.run)
      } yield res)

  def keepYoungest[M[_]: Monad, I](iGeneration: Lens[I, Long]): Objective[M, I] =
    Objective((clones: Vector[I]) => clones.maxBy(iGeneration.get).point[Vector].point[M])

  //TODO: L'algo suivant suppose que la partie de l'historique du jeune avant young.age est un duplicat du plus vieux.
  //Pas sur que ça soit toujours le cas: On peut avoir 2 clones qui ont évolué différemment et n'ont pas d'histoire commune.
  //Si c'est le cas, alors on supprime une partie d'historique que l'on devrait garder.
  def mergeHistories[M[_]: Monad, I, P](iAge: Lens[I, Long], iHistory: Lens[I, Vector[P]])(historySize: Int): Objective[M, I] =
    Objective((clones: Vector[I]) => {
      clones.sortBy { i => -iAge.get(i) }.reduceLeft { (i1, i2) =>
        val oldAge = iAge.get(i1)
        val youngAge = iAge.get(i2)

        def oldH: Vector[P] = iHistory.get(i1)
        def youngH: Vector[P] = iHistory.get(i2).takeRight(min(youngAge, Int.MaxValue).toInt)

        iAge.set(iHistory.set(i1, (oldH ++ youngH).takeRight(historySize)), oldAge + youngAge)
      }
    }.point[Vector].point[M])
}

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

import contexts._

import scala.math.min

object elitism {

  type Elitism[M[_], I] = Kleisli[M, Vector[I], Vector[I]]

  object Elitism {
    def apply[M[_], I](f: Vector[I] => M[Vector[I]])(implicit MM: Monad[M]): Elitism[M, I] = Kleisli(f)
  }

  def minimiseO[M[_], I, F](f: I => F, mu: Int)(implicit MM: Monad[M], FO: Order[F]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) => individuals.sorted(FO.contramap[I](f).toScalaOrdering).take(mu).point[M])

  def maximiseO[M[_], I, F](f: I => F, mu: Int)(implicit MM: Monad[M], FO: Order[F]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) => individuals.sorted(FO.contramap[I](f).reverseOrder.toScalaOrdering).take(mu).point[M])

  /** Returns n individuals randomly. */
  def randomO[M[_], I](n: Int)(implicit MM: Monad[M], MR: RandomGen[M]): Elitism[M, I] =
    Elitism(
      (individuals: Vector[I]) => {
        val popSize = individuals.size
        for {
          rg <- MR.random
        } yield Vector.fill(n)(individuals(rg.nextInt(popSize)))
      }
    )

  def incrementGeneration[M[_]: Monad, I](age: monocle.Lens[I, Long])(implicit MG: Generational[M]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) =>
      for {
        _ <- MG.incrementGeneration
      } yield individuals.map(age.modify(_ + 1))
    )

  def addHits[M[_]: Monad, I, C](cell: I => C, mapped: monocle.Lens[I, Boolean])(implicit MH: HitMapper[M, C]) =
    Kleisli.kleisli[M, Vector[I], Vector[I]] { (is: Vector[I]) =>
      for {
        _ <- MH.hits(is.filter(i => !mapped.get(i)).map(cell))
      } yield is.map(mapped.set(true))
    }

  /** Returns the mu individuals with the highest ranks. */
  // FIXME: unbiais when several individuals have the exact same rank (random draw)
  def keepHighestRanked[M[_], I, K](f: Kleisli[M, Vector[I], Vector[K]], mu: Int)(implicit MM: Monad[M], KO: Order[K]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) =>
      if (individuals.size < mu) individuals.point[M]
      else
        for {
          scores <- f.run(individuals)
        } yield {
          val sortedBestToWorst = (individuals zip scores).sortBy { _._2 }(KO.reverseOrder.toScalaOrdering).map { _._1 }
          sortedBestToWorst.take(mu)
        }
    )

  type CloneStrategy[M[_], I] = Vector[I] => M[Vector[I]]

  /**** Clone strategies ****/
  def applyCloneStrategy[M[_], I, G](getGenome: I => G, cloneStrategy: CloneStrategy[M, I])(implicit MM: Monad[M]): Elitism[M, I] =
    Elitism((individuals: Vector[I]) =>
      for {
        res <- individuals.groupBy(getGenome).valuesIterator.toVector.traverseM[M, I](is => cloneStrategy(is))
      } yield res
    )

  def keepYoungest[M[_]: Monad, I](age: I => Long): CloneStrategy[M, I] =
    (clones: Vector[I]) => clones.minBy(age).point[Vector].point[M]

  def mergeHistories[M[_]: Monad, I, P](age: monocle.Lens[I, Long], history: monocle.Lens[I, Vector[P]])(historySize: Int): CloneStrategy[M, I] =
    (clones: Vector[I]) => {
      clones.reduce { (i1, i2) =>
        val (old, young) = if (age.get(i1) > age.get(i2)) (i1, i2) else (i2, i1)

        val oldAge = age.get(old)
        val youngAge = age.get(young)

        def oldH: Vector[P] = history.get(old)
        def youngH: Vector[P] = history.get(young).takeRight(min(youngAge, historySize).toInt)
        def updatedHistory = history.set((oldH ++ youngH).takeRight(historySize))(old)
        age.set(oldAge + youngAge)(updatedHistory)
      }
    }.point[Vector].point[M]

}

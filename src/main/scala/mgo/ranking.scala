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
package mgo

import cats._
import cats.data._
import cats.implicits._
import tools.metric._
import tools.{ HierarchicalRanking, Lazy }
import freedsl.random._
import mgo.contexts._
import mgo.dominance._
import mgo.niche._
import mgo.tools.math._
import mgo.diversity._

import scala.Ordering.Implicits._
import scala.language.higherKinds

object ranking {

  /**
   * Compute the ranks of the individuals in the same order
   */
  type Ranking[M[_], I] = Kleisli[M, Vector[I], Vector[Lazy[Int]]]

  object Ranking {
    def apply[M[_]: Monad, I](f: Vector[I] => M[Vector[Lazy[Int]]]): Ranking[M, I] = Kleisli(f)
  }

  def monoObjectiveRanking[M[_]: Monad, I](fitness: I => Double): Ranking[M, I] =
    Ranking((values: Vector[I]) => {
      val byFitness = values.zipWithIndex.sortBy { case (i, id) => fitness(i) }.map { _._2 }
      byFitness.zipWithIndex.sortBy { case (id, _) => id }.map { case (_, rank) => Lazy(rank) }
    }.pure[M])

  def hyperVolumeRanking[M[_]: Monad, I](referencePoint: Vector[Double], fitness: I => Vector[Double]): Ranking[M, I] =
    Ranking((values: Vector[I]) =>
      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => fitness(e)), referencePoint)).pure[M])

  def hierarchicalRanking[M[_]: Monad, I](fitness: I => Vector[Double]): Ranking[M, I] =
    Ranking((values: Vector[I]) =>
      HierarchicalRanking.upRank(values.map(v => fitness(v))).pure[M])

  def paretoRanking[M[_]: Monad, I](fitness: I => Vector[Double], dominance: Dominance = nonStrictDominance): Ranking[M, I] =
    Ranking((values: Vector[I]) => {
      val fitnesses = values.map(i => fitness(i))

      fitnesses.zipWithIndex.map {
        case (v1, index1) =>
          Lazy(
            if (v1.exists(_.isNaN)) Int.MaxValue
            else {
              fitnesses.zipWithIndex.filter {
                case (_, index2) => index1 != index2
              }.count {
                case (v2, _) => dominance.isDominated(v1, v2)
              }
            }
          )
      }
    }.pure[M])

  def profileRanking[M[_]: Monad, I](niche: Niche[I, Int], fitness: I => Double): Ranking[M, I] =
    Ranking((population: Vector[I]) => {
      val (points, indexes) =
        population.map {
          i => (niche(i).toDouble, fitness(i))
        }.zipWithIndex.sortBy(_._1._1).unzip

      def signedSurface(p1: Point2D, p2: Point2D, p3: Point2D) = {
        val surface = mgo.tools.math.surface(p1, p2, p3)
        if (isUpper(p1, p3, p2)) -surface else surface
      }

      val contributions =
        points match {
          case Seq() => Seq.empty
          case Seq(x) => Seq(1.0)
          case s =>
            val first = s(0)
            val second = s(1)
            val zero = (first.x - (second.x - first.x), second.y)

            val leftSurface = signedSurface(zero, first, second)

            val preLast = s(s.length - 2)
            val last = s(s.length - 1)
            val postLast = (last.x + (last.x - preLast.x), preLast.y)

            val rightSurface = signedSurface(preLast, last, postLast)

            val middlePoints = s.sliding(3).filter(_.size == 3).map {
              s => signedSurface(s(0), s(1), s(2))
            }

            val surfaces = (Seq(leftSurface) ++ middlePoints ++ Seq(rightSurface)).zip(indexes).sortBy(_._2).map(_._1)
            val smallest = surfaces.min
            surfaces.map(s => s - smallest)
        }

      HierarchicalRanking.downRank(contributions.toVector)
    }.pure[M])

  //TODO: Lazy ne sert à rien ici. On pourrait redefinir le type Ranking en Ranking[M,I,K] avec K est de typeclass Order,
  // pour ne pas toujours être obligé d'utiliser Lazy.
  def hitCountRanking[M[_]: Monad, I, C](cell: I => C)(implicit hm: HitMap[M, C]): Ranking[M, I] = {
    def hitCount(cell: C) = hm.get.map(m => m.getOrElse(cell, 0))
    Ranking { is => is.traverse { i: I => hitCount(cell(i)).map[Lazy[Int]] { i => Lazy(i) } } }
  }

  /**** Generic functions on rankings ****/

  def reversedRanking[M[_]: Monad, I](ranking: Ranking[M, I]): Ranking[M, I] =
    Ranking((population: Vector[I]) => ranking(population).map { _.map { x => /*map lazyly*/ Lazy(-x()) } })

  //TODO: the following functions don't produce rankings and don't belong here.
  def rankAndDiversity[M[_]: Monad, I](ranking: Ranking[M, I], diversity: Diversity[M, I]): Kleisli[M, Vector[I], Vector[(Lazy[Int], Lazy[Double])]] =
    Kleisli((population: Vector[I]) =>
      for {
        r <- ranking(population)
        d <- diversity(population)
      } yield r zip d)

  def paretoRankingMinAndCrowdingDiversity[M[_]: Monad: Random, I](fitness: I => Vector[Double]): Kleisli[M, Vector[I], Vector[(Lazy[Int], Lazy[Double])]] =
    rankAndDiversity(
      reversedRanking(paretoRanking[M, I](fitness)),
      crowdingDistance[M, I] { (i: I) => fitness(i) }
    )

  def rank[M[_]: Monad, I, K](ranking: Kleisli[M, Vector[I], Vector[K]]) = Kleisli[M, Vector[I], Vector[(I, K)]] { is =>
    for {
      rs <- ranking.run(is)
    } yield is zip rs
  }

}

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

import fr.iscpif.mgo.Contexts._

import scala.language.higherKinds

import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.tools.Math._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo.tools.metric.Hypervolume
import Ordering.Implicits._
import fitness._
import niche._
import dominance._
import scalaz.Ordering.{ LT, GT, EQ }

import scala.util.Random
import scalaz._
import Scalaz._

object ranking {

  /**
   * Compute the ranks of the individuals in the same order
   */
  type Ranking[M[_], I] = Kleisli[M, Vector[I], Vector[Lazy[Int]]]
  object Ranking {
    def apply[M[_]: Monad, I](f: Vector[I] => M[Vector[Lazy[Int]]]): Ranking[M, I] = Kleisli(f)
  }

  implicit def monoObjectiveRanking[M[_]: Monad, I](fitness: Fitness[I, Double]): Ranking[M, I] =
    Ranking((values: Vector[I]) => {
      val byFitness = values.zipWithIndex.sortBy { case (i, id) => fitness(i) }.map { _._2 }
      byFitness.zipWithIndex.sortBy { case (id, _) => id }.map { case (_, rank) => Lazy(rank) }
    }.point[M])

  def hyperVolumeRanking[M[_]: Monad, I](referencePoint: Seq[Double], fitness: Fitness[I, Seq[Double]]): Ranking[M, I] =
    Ranking((values: Vector[I]) =>
      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => fitness(e)), referencePoint)).point[M])

  def hierarchicalRanking[M[_]: Monad, I](fitness: Fitness[I, Vector[Double]]): Ranking[M, I] =
    Ranking((values: Vector[I]) =>
      HierarchicalRanking.upRank(values.map(v => fitness(v))).point[M])

  def paretoRanking[M[_]: Monad, I](fitness: Fitness[I, Vector[Double]], dominance: Dominance = nonStrictDominance): Ranking[M, I] =
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
    }.point[M])

  def profileRanking[M[_]: Monad, I](niche: Niche[I, Int], fitness: Fitness[I, Double]): Ranking[M, I] =
    Ranking((population: Vector[I]) => {
      val (points, indexes) =
        population.map {
          i => (niche(i).toDouble, fitness(i))
        }.zipWithIndex.sortBy(_._1._1).unzip

      def signedSurface(p1: Point2D, p2: Point2D, p3: Point2D) = {
        val surface = Math.surface(p1, p2, p3)
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
    }.point[M])

  //TODO: Lazy ne sert à rien ici. On pourrait redefinir le type Ranking en Ranking[M,I,K] avec K est de typeclass Order,
  // pour ne pas toujours être obligé d'utiliser Lazy.
  def hitCountRanking[M[_], I, C](cell: I => C)(implicit MM: Monad[M], MH: HitMapper[M, C]): Ranking[M, I] =
    Ranking { is: Vector[I] => is.traverse { i: I => MH.hitCount(cell(i)).map[Lazy[Int]] { i => Lazy(i) } } }

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

  def paretoRankingMinAndCrowdingDiversity[M[_]: Monad: RandomGen, I](fitness: I => Vector[Double]): Kleisli[M, Vector[I], Vector[(Lazy[Int], Lazy[Double])]] =
    rankAndDiversity(
      reversedRanking(paretoRanking[M, I] { (i: I) => fitness(i) }),
      crowdingDistance[M, I] { (i: I) => fitness(i) })

  //TODO: on doit pouvoir supprimer cet instance d'order spécifique à (Lazy[Int],Lazy[Double]) en utilisant une instance
  //d'Order pour (A,B) et pour Lazy[A]
  implicit val orderRankAndDiversity: Order[(Lazy[Int], Lazy[Double])] = new Order[(Lazy[Int], Lazy[Double])] {
    def order(x: (Lazy[Int], Lazy[Double]), y: (Lazy[Int], Lazy[Double])): Ordering =
      if (x._1() < y._1()) LT
      else if (x._1() > y._1()) GT
      else if (x._2() < y._2()) LT
      else if (x._2() > y._2()) GT
      else EQ
  }

}

object rankingOld {

  import nicheOld._
  import fitnessOld._

  /**
   * Compute the ranks of the individuals in the same order
   */
  type Ranking[G, P] = Vector[Individual[G, P]] => Vector[Lazy[Int]]

  implicit def monoObjectiveRanking[G, P](fitness: Fitness[G, P, Double]): Ranking[G, P] =
    (values: Vector[Individual[G, P]]) => {
      val byFitness = values.zipWithIndex.sortBy { case (i, id) => fitness(i) }.map { _._2 }
      byFitness.zipWithIndex.sortBy { case (id, _) => id }.map { case (_, rank) => Lazy(rank) }
    }

  def hyperVolumeRanking[G, P](referencePoint: Seq[Double], fitness: Fitness[G, P, Seq[Double]]): Ranking[G, P] =
    (values: Vector[Individual[G, P]]) =>
      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => fitness(e)), referencePoint))

  def hierarchicalRanking[G, P](fitness: Fitness[G, P, Seq[Double]]): Ranking[G, P] =
    (values: Vector[Individual[G, P]]) =>
      HierarchicalRanking.upRank(values.map(v => fitness(v)))

  def paretoRanking[G, P](fitness: Fitness[G, P, Seq[Double]], dominance: Dominance = nonStrictDominance): Ranking[G, P] =
    (values: Vector[Individual[G, P]]) => {
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
    }

  def profileRanking[G, P](niche: Niche[G, P, Int], fitness: Fitness[G, P, Double]): Ranking[G, P] =
    (population: Vector[Individual[G, P]]) => {
      val (points, indexes) =
        population.map {
          i => (niche(i).toDouble, fitness(i))
        }.zipWithIndex.sortBy(_._1._1).unzip

      def signedSurface(p1: Point2D, p2: Point2D, p3: Point2D) = {
        val surface = Math.surface(p1, p2, p3)
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
    }
}

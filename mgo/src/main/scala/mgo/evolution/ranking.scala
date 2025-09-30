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
package mgo.evolution

import cats._
import cats.data._
import cats.implicits._
import mgo.evolution.algorithm.HitMap
import mgo.evolution.dominance._
import mgo.evolution.niche._
import mgo.tools._

import scala.language.higherKinds

object ranking {

  /**
   * Compute the ranks of the individuals in the same order
   */
  type Ranking[I] = Vector[I] => Vector[Later[Int]]

  object Ranking:
    def apply[I](f: Vector[I] => Vector[Later[Int]]): Ranking[I] = f

  def monoObjectiveRanking[I](fitness: I => Double): Ranking[I] =
    Ranking: values =>

      val byFitness = values.map(fitness).zipWithIndex.sortBy { case (v, _) => v }
      def ranks(fitnesses: List[Double], lastValue: Double = Double.NegativeInfinity, rank: Int = 0, rs: List[Int] = List()): List[Int] =
        fitnesses match
          case h :: t =>
            if (h > lastValue) ranks(t, h, rank + 1, rank :: rs)
            else ranks(t, h, rank, rank :: rs)
          case Nil => rs.reverse

      val ranksValue = ranks(byFitness.unzip._1.toList)

      (ranksValue zip byFitness.unzip._2).sortBy { case (_, r) => r }.unzip._1.toVector.map(r => Later(r))


  //  def hyperVolumeRanking[M[_]: cats.Monad, I](referencePoint: Vector[Double], fitness: I => Vector[Double]): Ranking[M, I] =
  //    Ranking((values: Vector[I]) =>
  //      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => fitness(e)), referencePoint)).pure[M])
  //
  //  def hierarchicalRanking[M[_]: cats.Monad, I](fitness: I => Vector[Double]): Ranking[M, I] =
  //    Ranking((values: Vector[I]) =>
  //      HierarchicalRanking.upRank(values.map(v => fitness(v))).pure[M])

  def numberOfDominating[I](fitness: I => Vector[Double], values: Vector[I], dominance: Dominance = nonStrictDominance): Vector[Later[Int]] =
    val fitnesses = values.map(i => fitness(i))
    def ranks =
      fitnesses.zipWithIndex.map: (v1, index1) =>
        def containsNaN = v1.exists(_.isNaN)
        def otherIndividuals = fitnesses.zipWithIndex.filter((_, index2) => index1 != index2)
        def numberOfDominatingIndividual = otherIndividuals.count((v2, _) => dominance.isDominated(v1, v2))
        Later:
          if containsNaN
          then Int.MaxValue
          else numberOfDominatingIndividual

    ranks

  //  def profileRanking[M[_]: cats.Monad, I](niche: Niche[I, Int], fitness: I => Double): Ranking[M, I] =
  //    Ranking((population: Vector[I]) => {
  //      val (points, indexes) =
  //        population.map {
  //          i => (niche(i).toDouble, fitness(i))
  //        }.zipWithIndex.sortBy(_._1._1).unzip
  //
  //      def signedSurface(p1: Point2D, p2: Point2D, p3: Point2D) = {
  //        val surface = mgo.tools.surface(p1, p2, p3)
  //        if (isUpper(p1, p3, p2)) -surface else surface
  //      }
  //
  //      val contributions =
  //        points match {
  //          case Seq() => Seq.empty
  //          case Seq(x) => Seq(1.0)
  //          case s =>
  //            val first = s(0)
  //            val second = s(1)
  //            val zero = (first.x - (second.x - first.x), second.y)
  //
  //            val leftSurface = signedSurface(zero, first, second)
  //
  //            val preLast = s(s.length - 2)
  //            val last = s(s.length - 1)
  //            val postLast = (last.x + (last.x - preLast.x), preLast.y)
  //
  //            val rightSurface = signedSurface(preLast, last, postLast)
  //
  //            val middlePoints = s.sliding(3).filter(_.size == 3).map {
  //              s => signedSurface(s(0), s(1), s(2))
  //            }
  //
  //            val surfaces = (Seq(leftSurface) ++ middlePoints ++ Seq(rightSurface)).zip(indexes).sortBy(_._2).map(_._1)
  //            val smallest = surfaces.min
  //            surfaces.map(s => s - smallest)
  //        }
  //
  //      HierarchicalRanking.downRank(contributions.toVector)
  //    }.pure[M])

  //TODO: Lazy ne sert à rien ici. On pourrait redefinir le type Ranking en Ranking[M,I,K] avec K est de typeclass Order,
  def hitCountRanking[S, I](s: S, population: Vector[I], cell: I => Vector[Int], hitmap: monocle.Lens[S, HitMap]): Vector[Int] = 
    def hitCount(cell: Vector[Int]): Int = hitmap.get(s).getOrElse(cell, 0)
    population.map { i => hitCount(cell(i)) }

  /**** Generic functions on rankings ****/

  def paretoRanking[I](population: Vector[I], fitness: I => Vector[Double], dominance: Dominance = nonStrictDominance): Vector[Eval[Int]] =
    numberOfDominating(fitness, population, dominance).map(_.map(x => -x))

  def paretoRankingMinAndCrowdingDiversity[I](population: Vector[I], fitness: I => Vector[Double]): Vector[(Eval[Int], Double)] =
    import mgo.tools.metric.CrowdingDistance
    paretoRanking(population, fitness) zip CrowdingDistance(population.map(fitness))

  def worstParetoRanking: (Later[Int], Double) = (Later(Int.MinValue), Double.NegativeInfinity)

  def rank[M[_]: cats.Monad, I, K](ranking: Kleisli[M, Vector[I], Vector[K]]): Kleisli[M, Vector[I], Vector[(I, K)]] = Kleisli[M, Vector[I], Vector[(I, K)]] { is =>
    for {
      rs <- ranking.run(is)
    } yield is zip rs
  }

}


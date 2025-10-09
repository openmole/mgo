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

import cats.*
import cats.data.*
import cats.implicits.*
import mgo.evolution.algorithm.HitMap
import mgo.evolution.dominance.*
import mgo.evolution.niche.*
import mgo.tools.*
import mgo.tools.metric.{CrowdingDistance, EskinDistance, GoodallDistance, WFGHypervolume}

import java.util
import scala.language.higherKinds

object ranking:

  def monoObjectiveRanking[I](population: Vector[I], fitness: I => Double) =
    val byFitness = population.map(fitness).zipWithIndex.sortBy { case (v, _) => v }
    def ranks(fitnesses: List[Double], lastValue: Double = Double.NegativeInfinity, rank: Int = 0, rs: List[Int] = List()): List[Int] =
      fitnesses match
        case h :: t =>
          if (h > lastValue) ranks(t, h, rank + 1, rank :: rs)
          else ranks(t, h, rank, rank :: rs)
        case Nil => rs.reverse

    val ranksValue = ranks(byFitness.unzip._1.toList)

    (ranksValue zip byFitness.unzip._2).sortBy { case (_, r) => r }.unzip._1.toVector

  def numberOfDominating[I](fitness: I => Vector[Double], values: Vector[I], dominance: Dominance = nonStrictDominance): Vector[Later[Int]] =
    val fitnesses = values.map(i => fitness(i)).toArray
    def ranks =
      fitnesses.zipWithIndex.map: (v1, vi) =>
        def containsNaN = v1.exists(_.isNaN)

        def numberOfDominatingIndividual =
          val size = fitnesses.length
          var dominating = 0
          Loop.loop(0, _ < size, _ + 1): i =>
            if i != vi && dominance.isDominated(v1, fitnesses(i))
            then dominating += 1
          dominating

        Later:
          if containsNaN
          then Int.MaxValue
          else numberOfDominatingIndividual

    ranks.toVector

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

  def paretoFronts[I](population: Vector[I], fitness: I => Vector[Double], dominance: Dominance = nonStrictDominance): Vector[Vector[I]] =
    @annotation.tailrec def loop(pool: Vector[I], acc: List[Vector[I]]): Vector[Vector[I]] =
      if pool.isEmpty
      then acc.toVector.reverse
      else
        val domCounts = numberOfDominating(fitness, pool, dominance).map(_.value)
        val (front, rest) =
          pool.zip(domCounts).partitionMap: (ind, d) =>
            if d == 0 then Left(ind) else Right(ind)

        loop(rest, front :: acc)

    loop(population, List())

  def genomicDiversity[I](population: Vector[I], values: I => (IArray[Double], IArray[Int])) =
    if population.isEmpty
    then Vector()
    else
      val (cSize, dSize) = 
        val (c, d) = values(population.head)
        (c.length, d.length)

      val continuousDiversity =
        if cSize != 0
        then CrowdingDistance.normalizedCrowdingDistance(population.map(i => values(i)._1.toVector))
        else population.map(_ => 0.0)

      val discreteDiversity =
        if dSize != 0
        then GoodallDistance.averageDiversity(population.map(i => values(i)._2))
        else population.map(_ => 0.0)

      (continuousDiversity zip discreteDiversity).map: (c, d) =>
        (c * cSize + d * dSize) / (cSize + dSize)


  def paretoRankingMinAndCrowdingDiversityWithGenomeDiversity[I](population: Vector[I], fitness: I => Vector[Double], values: I => (IArray[Double], IArray[Int])) =
    val gDiversity =
      import scala.jdk.CollectionConverters.*
      val map = new util.IdentityHashMap[I, Double]().asScala
      map.addAll:
        (population zip genomicDiversity(population, values))
      map

    def amplifiedFitness(i: I) = fitness(i) ++ Seq(-gDiversity(i))

    paretoRankingMinAndCrowdingDiversity(population, amplifiedFitness)



//  def byFrontsRankingMinAndCrowdingDiversity[I](population: Vector[I], fitness: I => Vector[Double], dominance: Dominance = nonStrictDominance): Vector[(Int, Double)] =
//    import mgo.tools.metric.CrowdingDistance
//    val fronts = paretoFronts(population, fitness, dominance)
//    fronts.zipWithIndex.flatMap: (f, i) =>
//      val diversity = CrowdingDistance(f.map(fitness))
//      Vector.fill(f.size)(-i) zip diversity
//
//  def paretoRankingMinAndHyperVolumeContributionDiversity[I](population: Vector[I], fitness: I => Vector[Double], dominance: Dominance = nonStrictDominance): Vector[(Int, Eval[Double])] =
//    paretoFronts(population, fitness, dominance).zipWithIndex.flatMap: (f, i) =>
//      val points = IArray.from(f.map(i => IArray.from(fitness(i))))
//      val contributions = WFGHypervolume.normalizedHypervolumeContribution(points)
//      Vector.fill(f.size)(-i) zip contributions

  def worstParetoRanking: (Later[Int], Double) = (Later(Int.MinValue), Double.NegativeInfinity)



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

  //  def hyperVolumeRanking[M[_]: cats.Monad, I](referencePoint: Vector[Double], fitness: I => Vector[Double]): Ranking[M, I] =
  //    Ranking((values: Vector[I]) =>
  //      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => fitness(e)), referencePoint)).pure[M])
  //
  //  def hierarchicalRanking[M[_]: cats.Monad, I](fitness: I => Vector[Double]): Ranking[M, I] =
  //    Ranking((values: Vector[I]) =>
  //      HierarchicalRanking.upRank(values.map(v => fitness(v))).pure[M])





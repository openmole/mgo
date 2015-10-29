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

import fr.iscpif.mgo.tools.Math._
import fr.iscpif.mgo.tools._
import fr.iscpif.mgo.tools.metric.Hypervolume
import Ordering.Implicits._

trait Ranking <: Pop {

  /**
    * Compute the ranks of the individuals in the same order
    */
  trait Ranking extends (Pop => Vector[Lazy[Int]])

}

trait RankingFunctions <: Ranking with Fitness with Niche {

  implicit def monoObjectiveRanking(implicit doubleFitness: Fitness[Double]) = new Ranking {
    def apply(values: Pop) = {
      val byFitness = values.zipWithIndex.sortBy { case (i, id) => doubleFitness(i) }.map { _._2 }
      byFitness.zipWithIndex.sortBy { case(id, _) => id }.map { case(_, rank) => Lazy(rank) }
    }
  }

  def hyperVolumeRanking(referencePoint: Seq[Double])(implicit mg: Fitness[Seq[Double]]) = new Ranking {
    override def apply(values: Pop): Vector[Lazy[Int]] =
      HierarchicalRanking.downRank(Hypervolume.contributions(values.map(e => mg(e)), referencePoint))
  }

  def hierarchicalRanking(implicit mg: Fitness[Seq[Double]]) =  new Ranking {
    override def apply(values: Pop): Vector[Lazy[Int]] =
      HierarchicalRanking.upRank(values.map(v => mg(v)))
  }

  def paretoRanking(dominance: Dominance = Dominance.nonStrictDominance)(implicit mg: Fitness[Seq[Double]]) = new Ranking {
    override def apply(values: Pop): Vector[Lazy[Int]] = {
      assert(mg != null)
      val fitnesses = values.map(i => mg(i))

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
  }


  def profileRanking(implicit niche: Niche[Int], aggregation: Fitness[Double]) = new Ranking {

    override def apply(population: Pop): Vector[Lazy[Int]] = {
      val (points, indexes) =
        population.map {
          i => (niche(i).toDouble, aggregation(i))
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

}

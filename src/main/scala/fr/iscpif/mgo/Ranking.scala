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

import fr.iscpif.mgo.tools._
import fr.iscpif.mgo.tools.metric.Hypervolume
import Ordering.Implicits._

trait Ranking <: Pop {

  /**
    * Compute the ranks of the individuals in the same order
    */
  trait Ranking extends (Pop => Vector[Lazy[Int]])

}

trait RankingFunctions <: Ranking with Fitness {

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
}

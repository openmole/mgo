/*
 * Copyright (C) 2012 Romain Reuillon
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

import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.tools.metric.Hypervolume.ReferencePoint
import fr.iscpif.mgo.tools.metric.{ CrowdingDistance, Hypervolume, KNearestNeighboursAverageDistance }
import fr.iscpif.mgo.Contexts._

import scala.util.Random
import scalaz._
import Scalaz._
import fitness._

/**
 * Layer of the cake that compute a diversity metric for a set of values
 */
object diversity {

  /** Compute the diversity metric of the values */
  type Diversity[M[_], I] = Kleisli[M, Vector[I], Vector[Lazy[Double]]]
  object Diversity {
    def apply[M[_], I](f: Vector[I] => M[Vector[Lazy[Double]]])(implicit MM: Monad[M]): Diversity[M, I] = Kleisli.kleisli[M, Vector[I], Vector[Lazy[Double]]](f)
  }

  /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance[M[_], I](fitness: Fitness[I, Seq[Double]])(implicit MM: Monad[M], MR: RandomGen[M]): Diversity[M, I] =
    Diversity((values: Vector[I]) =>
      for {
        rg <- MR.random
      } yield CrowdingDistance(values.map(e => fitness(e)))(rg))

  def hypervolumeContribution[M[_], I](referencePoint: ReferencePoint, fitness: Fitness[I, Seq[Double]])(implicit MM: Monad[M]): Diversity[M, I] =
    Diversity((values: Vector[I]) =>
      Hypervolume.contributions(values.map(e => fitness(e)), referencePoint).point[M])

  def KNearestNeighbours[M[_], I](k: Int, fitness: Fitness[I, Seq[Double]])(implicit MM: Monad[M]): Diversity[M, I] =
    Diversity((values: Vector[I]) =>
      KNearestNeighboursAverageDistance(values.map(e => fitness(e)), k).point[M])

}


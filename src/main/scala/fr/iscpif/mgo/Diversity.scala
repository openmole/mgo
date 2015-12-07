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

import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.tools.metric.Hypervolume.ReferencePoint
import fr.iscpif.mgo.tools.metric.{ CrowdingDistance, Hypervolume, KNearestNeighboursAverageDistance }

import scala.util.Random
import scalaz._
import fitness._

/**
 * Layer of the cake that compute a diversity metric for a set of values
 */
object diversity {

  /** Compute the diversity metric of the values */
  type Diversity[I] = Population[I] => Vector[Lazy[Double]]

  /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance[I](fitness: Fitness[I, Seq[Double]])(rg: Random): Diversity[I] =
    (values: Population[I]) =>
      CrowdingDistance(values.map(e => fitness(e)))(rg)

  def hypervolumeContribution[I](referencePoint: ReferencePoint, fitness: Fitness[I, Seq[Double]]): Diversity[I] =
    (values: Population[I]) =>
      Hypervolume.contributions(values.map(e => fitness(e)), referencePoint)

  def KNearestNeighbours[I](k: Int, fitness: Fitness[I, Seq[Double]]): Diversity[I] =
    (values: Population[I]) =>
      KNearestNeighboursAverageDistance(values.map(e => fitness(e)), k)

}

object diversityOld {
  import fitnessOld._

  /** Compute the diversity metric of the values */
  trait Diversity[G, P] extends (Population[Individual[G, P]] => State[Random, Vector[Lazy[Double]]])

  /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance[G, P](fitness: Fitness[G, P, Seq[Double]]) = new Diversity[G, P] {
    override def apply(values: Population[Individual[G, P]]) =
      State { (rg: Random) => (rg, CrowdingDistance(values.map(e => fitness(e)))(rg)) }
  }

  def hypervolumeContribution[G, P](referencePoint: ReferencePoint, fitness: Fitness[G, P, Seq[Double]]) = new Diversity[G, P] {
    override def apply(values: Population[Individual[G, P]]) =
      State.state { Hypervolume.contributions(values.map(e => fitness(e)), referencePoint) }
  }

  def KNearestNeighbours[G, P](k: Int, fitness: Fitness[G, P, Seq[Double]]) = new Diversity[G, P] {
    override def apply(values: Population[Individual[G, P]]) =
      State.state { KNearestNeighboursAverageDistance(values.map(e => fitness(e)), k) }
  }

}
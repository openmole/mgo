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

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy
import fr.iscpif.mgo.tools.metric.Hypervolume.ReferencePoint
import fr.iscpif.mgo.tools.metric.{KNearestNeighboursAverageDistance, Hypervolume, CrowdingDistance, ClosedCrowdingDistance}
import scala.util.Random
import scalaz._

/**
 * Layer of the cake that compute a diversity metric for a set of values
 */
trait Diversity <: Pop {
  /** Compute the diversity metric of the values */
  trait Diversity extends (Pop => State[Random, Vector[Lazy[Double]]])
}


trait DiversityDefault <: Diversity with Fitness {

 /* def closedCrowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { ClosedCrowdingDistance(values.map(e => mg(e))) }
  }*/

  def crowdingDistance(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      CrowdingDistance(values.map(e => mg(e)))
  }

  def hypervolumeContribution(referencePoint: ReferencePoint)(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { Hypervolume.contributions(values.map(e => mg(e)), referencePoint) }
  }

  def KNearestNeighbours(k: Int)(implicit mg: Fitness[Seq[Double]]) = new Diversity {
    override def apply(values: Pop) =
      State.state { KNearestNeighboursAverageDistance(values.map(e => mg(e)), k) }
  }

}
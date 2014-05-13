/*
 * Copyright (C) 2011 sebastien rey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools._
import scala.math._

/**
 * Diversity computed from an hypervolume contribution metric
 *
 * @see Hypervolume
 */
trait HypervolumeDiversity extends DiversityMetric with ReferencePoint {

  override def diversity(values: Seq[Seq[Double]]) = {
    val diversities = computeHypervolume(values.zipWithIndex.toIndexedSeq, referencePoint).sortBy { case (_, index) => index }
    diversities.map { case (contribution, _) => contribution }
  }

  /**
   * Compute the hypervolume contribution for each front
   */
  def computeHypervolume(front: IndexedSeq[(Seq[Double], Int)], referencePoint: Seq[Double]): IndexedSeq[(Lazy[Double], Int)] = {

    lazy val globalHypervolume = Hypervolume(front.map { e => e._1 }, referencePoint)

    //compute a new collection with automatic removed incremental of frontValues item by item
    front.shadows.zipWithIndex.map {
      case (e, indexShadowed) =>
        (Lazy(globalHypervolume - Hypervolume(e.map { _._1 }, referencePoint)), front(indexShadowed)._2)
    }
  }

}

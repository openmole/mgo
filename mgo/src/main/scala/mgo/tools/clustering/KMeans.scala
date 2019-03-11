/*
 * Copyright (C) 30/04/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mgo.tools.clustering

import math._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import mgo.tools.distance.Distance

trait KMeans <: Distance {

  def updateClusters(p: Seq[Cluster])(implicit rng: Random): Seq[Cluster] = {
    val ps = p.flatMap(_.points)
    val cs = p.map(_.centroid)
    createClusters(ps, cs)
  }

  def createClusters(points: Seq[Seq[Double]], centroids: Seq[Seq[Double]])(implicit rng: Random): Seq[Cluster] = {
    val assignations =
      for (p <- points) yield {
        val centroid = centroids.minBy(c => distance(p, c))
        centroid -> p
      }

    def ensureNumberOfCentroids(as: Seq[(Seq[Double], Seq[Double])]): Map[Seq[Double], Seq[Seq[Double]]] = {
      val map = as.groupBy(_._1).map {
        case (c, p) => c -> p.unzip._2
      }
      val missing = centroids.size - map.keys.size
      if (missing <= 0) map
      else {
        val shuffled = rng.shuffle(as)
        val reassing = shuffled.take(missing).map {
          case (_, p) => p -> p
        }
        ensureNumberOfCentroids(reassing.toList ::: shuffled.drop(missing).toList)
      }
    }

    ensureNumberOfCentroids(assignations).toList.map {
      case (_, ps) =>
        val c = centroid(ps)
        new Cluster {
          def centroid = c
          def points = ps
        }
    }
  }

  def centroid(p: Seq[Seq[Double]]) = p.transpose.map(c => c.sum / c.size)

  def initialCentroids(p: Seq[Seq[Double]])(implicit rng: Random): Seq[Seq[Double]]

  def compute(p: Seq[Seq[Double]])(implicit rng: Random): Seq[Cluster] = {
    def step(clusters: Seq[Cluster]): Seq[Cluster] = {
      val newClusters = updateClusters(clusters)
      if (stop(clusters, newClusters)) newClusters
      else step(updateClusters(clusters))
    }

    step(createClusters(p, initialCentroids(p)))
  }

  def stop(old: Seq[Cluster], current: Seq[Cluster]) = {
    import Ordering.Implicits._
    old.map(_.centroid).sorted == current.map(_.centroid).sorted
  }

}

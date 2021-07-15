/*
 * Copyright (C) 09/05/2014 Guillaume Chérel
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

package mgo.tools.metric

import cats._
import mgo.tools.KDTree

/**
 * Distance to the K Nearest Neighbours using the KD-Tree algorithm
 *
 */

object KNearestNeighboursAverageDistance {

  def apply(values: Vector[Seq[Double]], k: Int): Vector[Later[Double]] = {
    val tree = KDTree(values)

    values.map {
      v =>
        Later {
          val neighbours = tree.knearest(k, v)
          neighbours.foldLeft(0: Double) { case (sum, cur) => sum + tree.distance(cur.toSeq, v) } / neighbours.size
        }
    }
  }
}
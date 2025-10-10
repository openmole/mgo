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

object KNearestNeighboursAverageDistance:

//  def apply(values: Vector[Seq[Double]], k: Int): Vector[Later[Double]] =
//    val tree = KDTree(values)
//
//    values.map: v =>
//      Later:
//        val neighbours = tree.knearest(k, v)
//        neighbours.map(tree.distance(_, v)).sum / neighbours.size


  def normalizedDistance(values: Vector[Seq[Double]], k: Int): Seq[Double] =
    val distances = knnDistances(values, k)
    val max = distances.max
    distances.map(_ / max)

  def knnDistances(points: Seq[Seq[Double]], k: Int): Seq[Double] =
    require(points.nonEmpty, "Points cannot be empty")
    require(k > 0 && k < points.size, "k must be between 1 and number of points - 1")

    inline def euclidean(a: Seq[Double], b: Seq[Double]): Double =
      math.sqrt:
        (a lazyZip b).map: (x, y) =>
          val d = x - y
          d * d
        .sum

    val n = points.size
    // Mutable 2D array for storing distances
    val distMatrix = Array.ofDim[Double](n, n)

    // Compute only upper triangle (i < j)
    for
      i <- 0 until n
      j <- (i + 1) until n
    do
      val d = euclidean(points(i), points(j))
      distMatrix(i)(j) = d
      distMatrix(j)(i) = d

    // For each point, get k smallest non-zero distances
    (0 until n).map: i =>
      distMatrix(i)
        .zipWithIndex
        .filter(_._2 != i)
        .map(_._1)
        .sorted
        .take(k)
        .sum
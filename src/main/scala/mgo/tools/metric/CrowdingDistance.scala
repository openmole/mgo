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

package mgo.tools.metric

import mgo.tools._

import freedsl.random._
import cats._
import cats.implicits._
import shapeless.Lazy
/**
 * Crowding distance computation see Deb, K., Agrawal, S., Pratap, A. & Meyarivan, T.
 * A fast elitist non-dominated sorting genetic algorithm for multi-objective
 * optimization: NSGA-II. Lecture notes in computer science 1917, 849–858 (2000).
 */
object CrowdingDistance {

  /**
   * Compute the crowding distance
   *
   * @param data the set of point
   * @return the crowding distance of each point in the same order as the input
   * sequence
   */
  def apply[M[_]: cats.Monad](data: Vector[Vector[Double]])(implicit randomM: Random[M]): M[Vector[Lazy[Double]]] = {
    def res = data.transpose.map {
      d: Vector[Double] =>
        val grouped: Map[Double, Seq[Int]] =
          d.zipWithIndex.groupBy { case (d, _) => d }.mapValues { _.map { case (_, i) => i } }

        val sortedDistances = grouped.keys.toVector.sorted

        type Crowding = (Double, Int)

        def groupCrowding(group: Seq[Int], c: Double): M[List[(Double, Int)]] =
          randomM.nextInt(group.size).map { randomIndex =>
            (c -> group(randomIndex)) :: group.patch(randomIndex, Seq.empty, 1).toList.map { t => 0.0 -> t }
          }

        val res: M[Vector[Crowding]] =
          if (sortedDistances.size <= 2)
            sortedDistances.traverse {
              d => groupCrowding(grouped(d), Double.PositiveInfinity)
            }.map(_.flatten)
          else {
            def crowding(distances: List[Double], acc: List[Crowding]): M[List[Crowding]] =
              distances match {
                case d1 :: d2 :: Nil =>
                  for {
                    g1 <- groupCrowding(grouped(sortedDistances.head), Double.PositiveInfinity)
                    g2 <- groupCrowding(grouped(sortedDistances.last), Double.PositiveInfinity)
                  } yield g1 ::: (g2 ::: acc).reverse
                case d1 :: d2 :: d3 :: _ =>
                  for {
                    gc <- groupCrowding(grouped(d2), d3 - d1)
                    c <- crowding(distances.tail, gc ::: acc)
                  } yield c
                case _ => sys.error("Should never be empty")
              }

            crowding(sortedDistances.toList, List.empty).map(_.toVector)
          }
        res.map(_.sortBy { case (_, index) => index }.map { case (c, _) => c })
    }.sequence

    res.map(_.transpose.map { _.sum }.map(Lazy(_)))
  }

}

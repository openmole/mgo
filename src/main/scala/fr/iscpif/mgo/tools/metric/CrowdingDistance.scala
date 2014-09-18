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

package fr.iscpif.mgo.tools.metric

import fr.iscpif.mgo.tools.Lazy

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
  def apply(data: Seq[Seq[Double]]): Seq[Lazy[Double]] = {
    if (data.size <= 2) data.map(d => Lazy(Double.PositiveInfinity))
    else {
      data.transpose.map {
        d =>
          val (sorted, indices) = d.zipWithIndex.sortBy { case (d, _) => d }.unzip

          val head = sorted.head
          val last = sorted.last
          val diff = last - head

          def crowding(l: List[Double], acc: List[Double]): List[Double] =
            l match {
              case e1 :: e2 :: Nil => Double.PositiveInfinity :: (Double.PositiveInfinity :: acc).reverse
              case e1 :: e2 :: e3 :: _ =>
                val c = e3 - e1 / diff
                crowding(l.tail, c :: acc)
            }

          (crowding(sorted.toList, Nil) zip indices).sortBy { case (_, i) => i }.unzip._1
      }.transpose.map { _.sum }.map(Lazy(_))
    }
  }

}

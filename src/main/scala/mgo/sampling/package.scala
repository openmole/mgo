/*
 * Copyright (C) 2017 Guillaume Chérel, Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mgo

import util.Random

package object sampling {

  def lhs(dimensions: Int, samples: Int)(implicit rng: Random): Vector[Vector[Double]] =
    Vector.fill(dimensions) {
      rng.shuffle((0 until samples).toIterator).map {
        i => (i + rng.nextDouble) / samples
      }.toVector
    }.transpose

  /**
   * Returns as many matrices as there are colums in a (a and b must be the
   * same size), such that in the i-th matrice, all elements are from a except
   * the i-th column which is from b.
   */
  def replaceColumnsOneByOne[A](a: Vector[Vector[A]], b: Vector[Vector[A]]): Vector[Vector[Vector[A]]] = {
    val aT = a.transpose
    val bT = b.transpose
    Vector.tabulate(aT.size) {
      i => (aT.take(i) ++ Vector(bT(i)) ++ aT.drop(i + 1)).transpose
    }
  }

}

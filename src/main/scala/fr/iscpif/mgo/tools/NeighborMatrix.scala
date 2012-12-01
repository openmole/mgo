/*
 * Copyright (C) 14/11/12 Romain Reuillon
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

package fr.iscpif.mgo.tools

import Function._

object NeighborMatrix {

  def empty[S] =
    new NeighborMatrix {
      type T = S
      def maxX = 0
      def maxY = 0
      def matrix(x: Int, y: Int) = None
    }

  def apply[S](elements: (Int, Int) => Option[S], mX: Int, mY: Int) =
    new NeighborMatrix {
      type T = S
      def maxX = mX
      def maxY = mY
      def matrix(x: Int, y: Int) = elements(x, y)
    }

}

trait NeighborMatrix {
  type T

  def matrix(x: Int, y: Int): Option[T]
  def maxX: Int
  def maxY: Int

  def knn(x: Int, y: Int, n: Int) =
    growUntilEnough(x, y, n).sortBy { case (x1, y1) => distance(x, y, x1, y1) }.take(n)

  def distance(x1: Int, y1: Int, x2: Int, y2: Int) = math.hypot(x2 - x1, y2 - y1)

  def isIn(x: Int, y: Int) = {
    def isIn(c: Int, maxC: Int) = c >= 0 && c <= maxC
    isIn(x, maxX) && isIn(y, maxY)
  }

  lazy val maxRange = math.max(maxX / 2.0, maxY / 2.0)

  def growUntilEnough(x: Int, y: Int, n: Int, range: Int = 1): List[(Int, Int)] = {
    val included = (extrema(x, y, range) ::: square(x, y, range).toList).filter { case (x1, y1) => matrix(x1, y1).isDefined }
    if (included.size >= n || range > maxRange) included
    else growUntilEnough(x, y, n, range + 1)
  }

  def extrema(x: Int, y: Int, range: Int) =
    for {
      dx <- List(x - range - 1, x + range + 1)
      dy <- List(y - range - 1, y + range + 1)
      if isIn(dx, dy)
    } yield (dx, dy)

  def square(x: Int, y: Int, range: Int) =
    for {
      dx <- (x - range) to (x + range)
      dy <- (y - range) to (y + range)
      if !(dx == x && dy == y)
      if isIn(dx, dy)
    } yield (dx, dy)

}

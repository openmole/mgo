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

package mgo.tools

import Function._

object NeighborMatrix {

  def empty[S]: NeighborMatrix[S] =
    new NeighborMatrix[S] {
      def maxX = 0
      def maxY = 0
      def matrix(x: Int, y: Int) = None
    }

  def apply[S](elements: (Int, Int) => Option[S], mX: Int, mY: Int): NeighborMatrix[S] =
    new NeighborMatrix[S] {
      def maxX = mX
      def maxY = mY
      def matrix(x: Int, y: Int) = elements(x, y)
    }

  def apply[S](elements: Iterable[S], index: S => (Int, Int)): NeighborMatrix[S] = {
    val matrix = elements.map(e => index(e) -> e).toMap
    val maxX = matrix.keys.map(_._1).max + 1
    val maxY = matrix.keys.map(_._2).max + 1
    apply[S](untupled(matrix.get _), maxX, maxY)
  }

}

trait NeighborMatrix[T] {

  def matrix(x: Int, y: Int): Option[T]
  def maxX: Int
  def maxY: Int

  def knn(x: Int, y: Int, n: Int): List[(Int, Int)] =
    growUntilEnough(x, y, n).sortBy { case (x1, y1) => distance(x, y, x1, y1) }.take(n)

  def distance(x1: Int, y1: Int, x2: Int, y2: Int): Double = java.lang.Math.hypot(x2 - x1, y2 - y1)

  def isIn(x: Int, y: Int): Boolean = {
    def isIn(c: Int, maxC: Int) = c >= 0 && c <= maxC
    isIn(x, maxX) && isIn(y, maxY)
  }

  lazy val maxRange: Int = java.lang.Math.max(maxX, maxY)

  //TODO reuse previously found neighbours
  def growUntilEnough(x: Int, y: Int, n: Int, range: Int = 1): List[(Int, Int)] = {
    val included = (extrema(x, y, range) ::: square(x, y, range).toList).filter { case (x1, y1) => matrix(x1, y1).isDefined }
    if (included.size >= n || range > maxRange) included
    else growUntilEnough(x, y, n, range + 1)
  }

  def extrema(x: Int, y: Int, range: Int): List[(Int, Int)] =
    for {
      dx <- List(x - range - 1, x + range + 1)
      dy <- List(y - range - 1, y + range + 1)
      if isIn(dx, dy)
    } yield (dx, dy)

  def square(x: Int, y: Int, range: Int): IndexedSeq[(Int, Int)] =
    for {
      dx <- (x - range) to (x + range)
      dy <- (y - range) to (y + range)
      if !(dx == x && dy == y)
      if isIn(dx, dy)
    } yield (dx, dy)

}

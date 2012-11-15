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
  type COORDINATE = (Int, Int)

  implicit class Coordinate(val c: COORDINATE) extends AnyVal {
    def x = c._1
    def y = c._2
  }
}

import NeighborMatrix._

trait NeighborMatrix {
  type T

  def matrix: IndexedSeq[IndexedSeq[Seq[T]]]
  def maxX: Int
  def maxY: Int

  def coordinate(t: T): COORDINATE

  def knn(t: T, n: Int) =
    growUntilEnough(coordinate(t), n).sortBy(e => distance(t, e)).take(n)

  def distance(t1: T, t2: T) = {
    val (x1, y1) = coordinate(t1)
    val (x2, y2) = coordinate(t2)
    math.hypot(x2 - x1, y2 - y1)
  }

  def isIn(c: COORDINATE) = {
    def isIn(c: Int, maxC: Int) = c >= 0 && c < maxC
    isIn(c.x, maxX) && isIn(c.y, maxY)
  }

  lazy val maxRange = math.max(maxX / 2.0, maxY / 2.0)

  def growUntilEnough(c: COORDINATE, n: Int, range: Int = 1): List[T] = {
    val included = (extrema(c, range) ::: square(c, range).toList).flatMap{case(x, y) => matrix(x)(y)}
    if(included.size >= n || range > maxRange) included
    else growUntilEnough(c, n, range + 1)
  }

  def extrema(c: COORDINATE, range: Int) =
    for {
      dx <- List(c.x - range - 1, c.x + range + 1)
      dy <- List(c.y - range - 1, c.y + range + 1)
      if isIn(dx, dy)
    } yield(dx, dy)

  def square(c: COORDINATE, range: Int) =
    for {
      dx <- (c.x - range) to (c.x + range)
      dy <- (c.y - range) to (c.y + range)
      if isIn (dx, dy)
    } yield(dx, dy)



}

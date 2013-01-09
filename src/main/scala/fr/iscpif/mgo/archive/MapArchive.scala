/*
 * Copyright (C) 19/11/12 Romain Reuillon
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

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._
import tools._

trait MapArchive extends Archive with MapPlotter with Aggregation {
  type A = Array[Array[Int]]

  def initialArchive: A = Array.empty

  def toArchive(individuals: Seq[Individual[G, P, F]]): A = {
    val sparse = individuals.groupBy(plot).map {
      case (k, v) => k -> v.size
    }

    val maxX = sparse.keys.map(_._1).max + 1
    val maxY = sparse.keys.map(_._2).max + 1

    Array.tabulate[Int](maxX, maxY) { case (x, y) => sparse.getOrElse((x, y), 0) }
  }

  def combine(a1: A, a2: A): A =
    (a1.toIterable merge a2)(
      (l1, l2) => (l1.toIterable merge l2)(_ + _).toArray
    ).toArray

  def diff(original: A, modified: A) =
    (modified.toIterable merge original)(
      (l1, l2) => (l1.toIterable merge l2)(_ - _).toArray
    ).toArray

}

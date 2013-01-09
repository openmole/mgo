/*
 * Copyright (C) 07/01/13 Romain Reuillon
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

trait ProfileArchive extends Archive with ProfilePlotter with Aggregation {
  type A = Array[Int]

  def initialArchive: A = Array.empty

  def toArchive(individuals: Seq[Individual[G, P, F]]): A = {
    val indexed: Map[Int, Int] = individuals.groupBy(plot).map {
      case (k, v) => k -> v.size
    }
    val size = indexed.map(_._1).max + 1
    Array.tabulate(size)(i => indexed.getOrElse(i, 0))
  }

  def combine(a1: A, a2: A): A = (a1.toIterable merge a2)(_ + _).toArray

  def diff(original: A, modified: A) = (modified.toIterable merge original)(_ - _).toArray
}

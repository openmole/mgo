/*
 * Copyright (C) 2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package fr.iscpif.mgo.tools.network

import collection.immutable.IntMap

trait SparseTopology[E] {
  def in(u: Int): Vector[(Int, E)] = mapin(u).toVector
  def out(u: Int): Vector[(Int, E)] = mapout(u).toVector
  def edge(u: Int, v: Int): Option[E] = if ((mapout contains u) && (mapout(u) contains v)) Some(mapout(u)(v)) else None
  def iteredges: Iterator[(Int, Int, E)] =
    if (mapout.isEmpty)
      Iterator.empty
    else
      mapout.iterator.flatMap {
        case (node1, mapoutnode1) =>
          mapoutnode1.iterator.map { case (node2, e) => (node1, node2, e) }
      }

  def mapin: IntMap[IntMap[E]] //mapin(u) edges leading into u
  def mapout: IntMap[IntMap[E]] //mapout(u) edges leading out of u
}

object SparseTopology {

  def mapinFrom[E](s: Traversable[(Int, Int, E)]): IntMap[IntMap[E]] = {
    IntMap.empty[IntMap[E]] ++ (s.groupBy { _._2 }.map {
      case (outnode, edges) =>
        (outnode -> innodesAndEdges(edges))
    })
  }

  def mapoutFrom[E](s: Traversable[(Int, Int, E)]): IntMap[IntMap[E]] = {
    IntMap.empty[IntMap[E]] ++ (s.groupBy { _._1 }.map {
      case (innode, edges) =>
        (innode -> outnodesAndEdges(edges))
    })
  }

  def innodesAndEdges[E](s: Traversable[(Int, Int, E)]): IntMap[E] = IntMap.empty[E] ++ (s map { e => e._1 -> e._3 })
  def outnodesAndEdges[E](s: Traversable[(Int, Int, E)]): IntMap[E] = IntMap.empty[E] ++ (s map { e => e._2 -> e._3 })

}
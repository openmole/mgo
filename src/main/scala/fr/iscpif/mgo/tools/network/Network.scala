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

/**
 * N = Node data type
 * E = Edge data type
 */
trait Network[N, E] {
  def node(u: Int): N = nodes(u)
  def iternodes: Iterator[(Int, N)] = nodes.indices.iterator zip nodes.iterator

  def nodes: Vector[N]
  def edge(u: Int, v: Int): Option[E]
  def iteredges: Iterator[(Int, Int, E)]
}

object Network {
  def directedSparse[N, E](
    _nodes: Vector[N],
    _edges: Traversable[(Int, Int, E)]): Network[N, E] with DirectedEdges[E] with SparseTopology[E] =
    new Network[N, E] with DirectedEdges[E] with SparseTopology[E] {
      val nodes = _nodes
      val mapin = SparseTopology.mapinFrom(_edges)
      val mapout = SparseTopology.mapoutFrom(_edges)
    }
}


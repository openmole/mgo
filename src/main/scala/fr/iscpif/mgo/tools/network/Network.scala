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

/**
 * N = Node data type
 * E = Edge data type
 */
trait Network[N, E] {
  def node(u: Int): N = nodes(u)
  def iternodes: Iterator[(Int, N)] = nodes.indices.iterator zip nodes.iterator

  def nodes: IndexedSeq[N]
  def edge(u: Int, v: Int): Option[E]
  def iteredges: Iterator[(Int, Int, E)]
}

trait DirectedEdges[E] {
  def outedges(u: Int): Seq[(Int, Int, E)] = out(u) map { case (v, d) => (u, v, d) }
  def inedges(u: Int): Seq[(Int, Int, E)] = in(u) map { case (v, d) => (v, u, d) }
  def outneighbours(u: Int): Seq[Int] = out(u) map { _._1 }
  def inneighbours(u: Int): Seq[Int] = in(u) map { _._1 }

  def in(u: Int): Seq[(Int, E)]
  def out(u: Int): Seq[(Int, E)]
}

trait UndirectedEdges[E] {
  def edges(u: Int): Seq[(Int, Int, E)] = out(u) map { case (v, d) => (u, v, d) }
  def neighbours(u: Int): Seq[Int] = out(u) map { _._1 }

  def out(u: Int): Seq[(Int, E)]
}

object UndirectedEdges {
  /** Takes a sequence of edges and returns a sequence by adding a (n2,n1,e) for all (n1,n2,e) */
  def makeSymetric[E](s: Seq[(Int, Int, E)]) = (s.toSet ++ s.map { case (n1, n2, e) => (n2, n1, e) }).toSeq
}

trait SparseTopology[E] {
  def in(u: Int): Seq[(Int, E)] = mapin(u).toSeq
  def out(u: Int): Seq[(Int, E)] = mapout(u).toSeq
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

  def mapinFromSeq[E](s: Seq[(Int, Int, E)]): IntMap[IntMap[E]] = {
    IntMap.empty[IntMap[E]] ++ (s.groupBy { _._2 }.map {
      case (outnode, edges) =>
        (outnode -> innodesAndEdges(edges))
    })
  }

  def mapoutFromSeq[E](s: Seq[(Int, Int, E)]): IntMap[IntMap[E]] = {
    IntMap.empty[IntMap[E]] ++ (s.groupBy { _._1 }.map {
      case (innode, edges) =>
        (innode -> outnodesAndEdges(edges))
    })
  }

  def innodesAndEdges[E](s: Seq[(Int, Int, E)]): IntMap[E] = IntMap.empty[E] ++ (s map { e => e._1 -> e._3 })
  def outnodesAndEdges[E](s: Seq[(Int, Int, E)]): IntMap[E] = IntMap.empty[E] ++ (s map { e => e._2 -> e._3 })

}

trait DenseTopology[E] {
  def in(u: Int): Seq[(Int, E)] = (matrix zipWithIndex) map { case (row, v) => (v, row(u)) }
  def out(u: Int): Seq[(Int, E)] = (matrix(u) zipWithIndex) map { case (d, v) => (v, d) }
  def edge(u: Int, v: Int): Option[E] = Some(matrix(u)(v))
  def iteredges: Iterator[(Int, Int, E)] =
    matrix.iterator.zipWithIndex.flatMap {
      case (row, u) =>
        row.iterator.zipWithIndex.map { case (e, v) => (u, v, e) }
    }

  def matrix: Array[Array[E]]
}

// trait DirectedSparseNetwork[N, E] <: Network[N, E] with DirectedEdges[E] with SparseTopology[E] {
//   def node(u: Int): N
//   def edge(u: Int, v: Int): Option[E]
// }

// object DirectedSparseNetwork {
//   def apply(): :

// }

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

trait UndirectedEdges[E] {
  def edges(u: Int): Vector[(Int, Int, E)] = out(u) map { case (v, d) => (u, v, d) }
  def neighbours(u: Int): Vector[Int] = out(u) map { _._1 }

  val dotGraphType: String = "graph"
  val dotEdgeOperator: String = "--"

  def out(u: Int): Vector[(Int, E)]
}

object UndirectedEdges {
  /** Takes a sequence of edges and returns a sequence by adding a (n2,n1,e) for all (n1,n2,e) */
  def makeSymetric[E](s: Seq[(Int, Int, E)]): Vector[(Int, Int, E)] = (s.toSet ++ s.map { case (n1, n2, e) => (n2, n1, e) }).toVector
}

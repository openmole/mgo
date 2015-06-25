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

trait DirectedEdges[E] {
  def outedges(u: Int): Vector[(Int, Int, E)] = out(u) map { case (v, d) => (u, v, d) }
  def inedges(u: Int): Vector[(Int, Int, E)] = in(u) map { case (v, d) => (v, u, d) }
  def outneighbours(u: Int): Vector[Int] = out(u) map { _._1 }
  def inneighbours(u: Int): Vector[Int] = in(u) map { _._1 }

  def in(u: Int): Vector[(Int, E)]
  def out(u: Int): Vector[(Int, E)]
}

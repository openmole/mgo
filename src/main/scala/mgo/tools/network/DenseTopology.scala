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

package mgo.tools.network

trait DenseTopology[E] {
  def in(u: Int): Vector[(Int, E)] = (matrix.zipWithIndex) map { case (row, v) => (v, row(u)) }
  def out(u: Int): Vector[(Int, E)] = (matrix(u).zipWithIndex) map { case (d, v) => (v, d) }
  def edge(u: Int, v: Int): Option[E] = Some(matrix(u)(v))
  def iteredges: Iterator[(Int, Int, E)] =
    matrix.iterator.zipWithIndex.flatMap {
      case (row, u) =>
        row.iterator.zipWithIndex.map { case (e, v) => (u, v, e) }
    }

  def matrix: Vector[Vector[E]]
}

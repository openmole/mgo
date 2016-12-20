/*
 * Copyright (C) 24/03/13 Romain Reuillon
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

object HierarchicalRanking {

  def downRank[T](values: Vector[T])(implicit ordering: Ordering[T]) = upRank(values)(ordering.reverse)

  def upRank[T](values: Vector[T])(implicit ordering: Ordering[T]) =
    values.
      zipWithIndex.
      sortBy { case (v, _) => v }.
      map { case (_, originalOrder) => originalOrder }.
      zipWithIndex.
      sortBy { case (originalOrder, _) => originalOrder }.
      map(_._2).map(Lazy(_))

}


/**
 * Created by Romain Reuillon on 08/01/16.
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
 *
 */
package fr.iscpif.mgo.tools

object CanBeNaN {

  implicit def doubleCanBeNaN = new CanBeNaN[Double] {
    def isNaN(t: Double) = t.isNaN
  }

  implicit def vectorCanBeNaN[T](implicit cbn: CanBeNaN[T]) = new CanBeNaN[Vector[T]] {
    def isNaN(t: Vector[T]) = t.exists(cbn.isNaN)
  }
}

trait CanBeNaN[T] {
  def isNaN(t: T): Boolean
}
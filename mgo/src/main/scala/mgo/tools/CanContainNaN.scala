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
package mgo.tools

object CanContainNaN:
  given CanContainNaN[Double] = _.isNaN
  given [T](using cbn: CanContainNaN[T]): CanContainNaN[Vector[T]] = _.exists(cbn.containsNaN)
  given [T](using cbn: CanContainNaN[T]): CanContainNaN[IArray[T]] = _.exists(cbn.containsNaN)

trait CanContainNaN[T]:
  def containsNaN(t: T): Boolean

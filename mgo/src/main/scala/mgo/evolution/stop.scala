/**
 * Created by reuillon on 07/01/16.
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

package mgo.evolution

import cats.data._
import cats.implicits._
import squants.time._

object stop {

  type StopCondition[S, I] = (S, Vector[I]) => Boolean

  def afterGeneration[S, I](g: Long, generation: monocle.Lens[S, Long]): StopCondition[S, I] =
    (s, is) => generation.get(s) >= g

  def afterDuration[S, I](d: Time, start: monocle.Lens[S, Long]): StopCondition[S, I] =
    (s, is) => {
      val now = java.lang.System.currentTimeMillis()
      val st = start.get(s)
      (st + d.toMilliseconds) <= now
    }

  def never[S, I]: StopCondition[S, I] = (s, i) => false

}

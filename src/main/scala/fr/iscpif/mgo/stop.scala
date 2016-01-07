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

package fr.iscpif.mgo

import fr.iscpif.mgo.contexts._
import scala.concurrent.duration._
import scalaz._
import Scalaz._

object stop {

  type StopCondition[M[_], I] = Kleisli[M, Vector[I], Boolean]

  def afterGeneration[M[_]: Monad, I](g: Long)(implicit mGeneration: Generational[M]): StopCondition[M, I] =
    Kleisli.kleisli[M, Vector[I], Boolean] { (individuals: Vector[I]) =>
      for {
        cg <- mGeneration.getGeneration
      } yield cg >= g
    }

  def afterDuration[M[_]: Monad, I](d: Duration)(implicit mStartTime: StartTime[M]): StopCondition[M, I] =
    Kleisli.kleisli[M, Vector[I], Boolean] { (individuals: Vector[I]) =>
      for {
        st <- mStartTime.startTime
      } yield (d.toMillis + st) <= System.currentTimeMillis
    }
}

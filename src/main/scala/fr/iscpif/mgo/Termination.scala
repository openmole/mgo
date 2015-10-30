/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.mgo

import scala.concurrent.duration._
import scalaz._

object Termination {
  sealed trait Generation
  sealed trait Start
}

import Termination._

trait Termination {

  def afterStep[S](max: Int)(implicit step: monocle.Lens[S, Long @@ Generation]) = State { state: S => (state, step.get(state) >= max) }

  def afterTime[S](max: Duration)(implicit time: monocle.Lens[S, Long @@ Start]) = State {
    state: S =>
      val duration = (System.currentTimeMillis - time.get(state)).millis
      (state, duration >= max)
  }

  def never[S] = State { state: S => (state, false) }

}

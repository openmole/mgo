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
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.tools

/**
 * trait defining stateful computations
 * @tparam S the state type
 * @tparam R the result type
 */
case class StateMonad[R, S](
    runstate: S => (R, S)) { stmon =>

  def bind[T](f: R => StateMonad[T, S]): StateMonad[T, S] =
    new StateMonad[T, S]({
      s =>
        val (a, newState) = runstate(s)
        val StateMonad(g) = f(a)
        g(newState)
    })
}

object StateMonad {
  def pure[R, S](r: R): StateMonad[R, S] = StateMonad[R, S]({ s => (r, s) })
}

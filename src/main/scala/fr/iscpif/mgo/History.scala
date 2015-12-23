/*
 * Copyright (C) 23/12/2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

import scala.language.higherKinds
import scalaz._
import Scalaz._

trait History[P, I] {
  val lens: Lens[I, Vector[P]]
}

case class HistoryOps[P, I](self: I)(implicit I: History[P, I]) {
  def get: Vector[P] = I.lens.get(self)
  def set(h: Vector[P]): I = I.lens.set(self, h)
  def mod(f: Vector[P] => Vector[P]): I = I.lens.mod(f, self)
}

object ToHistoryOps {
  implicit def toHistoryOps[P, I](v: I)(implicit I: History[P, I]): HistoryOps[P, I] = HistoryOps[P, I](v)
}
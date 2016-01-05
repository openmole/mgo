/*
 * Copyright (C) 04/12/2015 Guillaume Chérel
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

import scala.util.Random

object expressions {
  type Expression[G, P] = G => P

  def historyE[G, P](gHistory: Lens[G, Vector[P]])(e: Expression[G, P]): Expression[G, G] =
    (g: G) => gHistory.mod(_ :+ e(g), g)

  def asE[G, G1, P1, P](gtog1: G => G1, p1top: P1 => P, e: Expression[G1, P1]): Expression[G, P] =
    (g: G) => p1top(e(gtog1(g)))

}

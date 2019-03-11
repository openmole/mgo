/*
 * Copyright (C) 29/01/2018 Guillaume Chérel
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

package mgo.tools.execution

import org.apache.commons.math3.random.RandomGenerator
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }

import mgo.abc._

case class Sequential[S](
  init: () => S,
  step: S => S,
  stop: S => Boolean) {

  def run: S = {

    @tailrec
    def go(s: S): S =
      if (stop(s)) s
      else go(step(s))

    go(init())
  }

  def scan: Vector[S] = {
    @tailrec
    def go(s: S, acc: Vector[S]): Vector[S] =
      if (stop(s)) { acc :+ s }
      else {
        val nextS = step(s)
        go(nextS, acc :+ s)
      }

    go(init(), Vector.empty)
  }
}

/*
 * Copyright (C) 2019 Guillaume Chérel
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
package mgo.tools.execution

import scala.util.Random

case class ExposedEval[S, SInit, SStep, X, Y](
  initPreEval: () => (SInit, Vector[X]),
  initPostEval: (SInit, Vector[Y]) => S,
  stepPreEval: S => (SStep, Vector[X]),
  stepPostEval: (SStep, Vector[Y]) => S,
  stop: S => Boolean) {

  def init(f: X => Y): S = {
    val (pass, xs) = initPreEval()
    val ys = xs.map(f)
    initPostEval(pass, ys)
  }
  def step(f: X => Y)(s: S): S = {
    val (pass, xs) = stepPreEval(s)
    val ys = xs.map(f)
    stepPostEval(pass, ys)
  }

  def run(f: X => Y): S = {
    def go(s: S): S = if (stop(s)) s else go(step(f)(s))
    go(init(f))
  }

  def scan(f: X => Y): Vector[S] = {
    def go(s: S): Vector[S] = if (stop(s)) Vector(s) else s +: go(step(f)(s))
    go(init(f))
  }

}


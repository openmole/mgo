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

import scala.annotation.tailrec
import scala.util.Random

/**
 * An datastructure describing an computation which at some points delegates
 * some work to the user. The user receives an intermediate state
 * (type SI) and a value of type X by calling pre.
 * The value needs to be transformed into another of type
 * Y that is fed back to the computation, along with the intermediate
 * state with the functions post. e.g. pseudo-code to
 * run the whole computation:
 *
 * (si, x) = pre()
 * y = f(x)
 * result = post(si, y)
 *
 */
case class ExposedEval[X, Y, S, U, V](
  pre: X => (S, U),
  post: (S, V) => Y) {

  def run(f: U => V)(s: X): Y = {
    val (pass, x) = pre(s)
    val ys = f(x)
    post(pass, ys)
  }
}


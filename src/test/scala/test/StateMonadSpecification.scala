/*
 * Copyright (C) Guillaume Chérel 2015
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

package test

import fr.iscpif.mgo.tools.StateMonad
import org.scalacheck.Prop._
import org.scalacheck.Properties

object StateMonadSpecification extends Properties("StateMonad") {

  property("left identity") = {
    val f = { x: Int => StateMonad[Int, Int](s => (x * s, s + 1)) }
    forAll { (x: Int, s: Int) =>
      StateMonad.pure(x).bind(f).runstate(s) ?= f(x).runstate(s)
    }
  }

  property("right identity") = {
    val f = { s: Int => (s * 2, s + 1) }
    val m = StateMonad(f)
    forAll { (s: Int) =>
      m.bind(StateMonad.pure).runstate(s) ?= m.runstate(s)
    }
  }

  property("associativity") = {
    val m = StateMonad.pure[Int, Int](5)
    val f = { x: Int => StateMonad[Int, Int](s => (x * s, s + 1)) }
    val g = { x: Int => StateMonad[Int, Int](s => (x - s, s - 1)) }
    forAll { s: Int =>
      (m.bind(f)).bind(g).runstate(s) ?= m.bind(f(_).bind(g)).runstate(s)
    }
  }

}

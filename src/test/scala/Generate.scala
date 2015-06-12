/*
 * Copyright (C) Guillaume Chérel 30/04/14
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

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Generate {
  val smallInt = Gen.choose(-10, 10)
  val reasonableInt = Gen.choose(-10000, 10000)
  def shuffle[T](s: Seq[T], prefix: Seq[T] = Vector()): Gen[Seq[T]] =
    if (s.length == 0) Gen.const(prefix.toSeq)
    else
      Gen.choose(0, s.length - 1)
        .flatMap { i => shuffle(s.take(i) ++ s.takeRight(s.length - i - 1), prefix :+ s(i)) }
}

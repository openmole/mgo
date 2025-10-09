package mgo.tools

/*
 * Copyright (C) 2025 Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object ImplementEqualMethod:
  inline given ImplementEqualMethod[Int] = identity
  inline given ImplementEqualMethod[Double] = identity

  given [T: ImplementEqualMethod as imeq]: ImplementEqualMethod[IArray[T]] = a => IArray.genericWrapArray(a.map(imeq.apply))
  given [T: ImplementEqualMethod as imeq]: ImplementEqualMethod[Vector[T]] = v => v.map(imeq.apply)
  given [T1: ImplementEqualMethod as eq1, T2: ImplementEqualMethod as eq2]: ImplementEqualMethod[(T1, T2)] = t => (eq1(t._1), eq2(t._2))

  opaque type EQM[T] = Any

  def apply[T: ImplementEqualMethod as eq](t: T) = eq(t)

trait ImplementEqualMethod[T]:
  def apply(t: T): ImplementEqualMethod.EQM[T]

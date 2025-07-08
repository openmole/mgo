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

import scala.reflect.ClassTag

object Loop:

  /** standard C-style for loop */
  inline def loop[A](
    inline start: A,
    inline condition: A => Boolean,
    inline advance: A => A)(inline loopBody: A => Any): Unit =
    var a = start
    while condition(a) do
      loopBody(a)
      a = advance(a)

  inline def map[A, B: ClassTag](a: IArray[A])(f: A => B) =
    val length = a.length
    val result = Array.ofDim[B](length)
    loop(0, _ < length, _ + 1): i =>
      result(i) = f(a(i))
    IArray.unsafeFromArray(result)

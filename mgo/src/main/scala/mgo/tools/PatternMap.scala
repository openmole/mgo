package mgo.tools

import mgo.tools

import java.util

/*
 * Copyright (C) 2026 Romain Reuillon
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

object PatternMap:
  final case class ArrayWrapper(array: Array[Int]):
    override def equals(obj: Any): Boolean =
      obj match
        case other: ArrayWrapper => java.util.Arrays.equals(array, other.array)
        case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(array)
    
    override def toString: String = array.mkString("ArrayWrapper(", ", ", ")")

  def empty[V]: PatternMap[V] = Map.empty

  extension [V](map: PatternMap[V])
    def get(key: Vector[Int]): Option[V] = map.get(ArrayWrapper(key.toArray))
    def getOrElse(key: Vector[Int], v: V): V = map.getOrElse(ArrayWrapper(key.toArray), v)
    def removed(key: Vector[Int]): PatternMap[V] = map.removed(ArrayWrapper(key.toArray))
    def updatedWith(key: Vector[Int])(f: Option[V] => Option[V]): PatternMap[V] = map.updatedWith(PatternMap.ArrayWrapper(key.toArray))(f)
    def updated(key: Vector[Int], value: V): PatternMap[V] = map.updated(ArrayWrapper(key.toArray), value)
    def contains(key: Vector[Int]): Boolean = map.contains(PatternMap.ArrayWrapper(key.toArray))
    def values: Iterable[V] = map.values
    def toMap: Map[Vector[Int], V] = map.map((k, v) => k.array.toVector -> v)


opaque type PatternMap[V] = Map[PatternMap.ArrayWrapper, V]







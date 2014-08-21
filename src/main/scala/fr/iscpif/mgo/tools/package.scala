/*
 * Copyright (C) 09/01/13 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo

import scala.util.Random

package object tools {

  implicit class ArrayDecorator[A](array: Array[A]) {
    def get(i: Int) =
      if (i < array.size) Some(array(i)) else None

  }

  implicit class IteratorDecorator[A](i: Iterator[A]) {
    def nextOption =
      if (i.hasNext) Some(i.next) else None
  }

  implicit class IterableDecorator[A](t: Iterable[A]) {
    def merge(t2: Iterable[A])(op: (A, A) => A) = {
      val size = math.max(t.size, t2.size)

      val i1 = t.iterator
      val i2 = t2.iterator

      (0 until size) map {
        i =>
          val x = i1.nextOption
          val y = i2.nextOption

          (x, y) match {
            case (Some(v1), Some(v2)) => op(v1, v2)
            case (o1, o2) => o1 orElse o2 get
          }

      }
    }
  }

  implicit class SeqDecorator[A](xs: Seq[A]) {
    /**
     * Shadow each element of a set
     */
    def shadows[B] =
      for (i <- xs.indices; (as, bs) = xs splitAt i) yield as ++ bs.tail

    def random(implicit rng: Random) = xs(rng.nextInt(xs.size))

  }

  def rndmChoice[T](t1: T, t2: T)(implicit rng: Random): T = {
    if (rng.nextDouble < 0.5) t1 else t2
  }
  //implicit def lazyOrdering[T](implicit ord: Ordering[T]) = tools.Lazy.lazyOrdering(ord)

  /*implicit class OptionDecorator[A](option: Option[A]) {
    def getOrElse(a: A) =
      option match {
        case Some(v) => v
        case None => a
      }
  }     */

}

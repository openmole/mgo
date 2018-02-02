/*
 * Copyright (C) 2015 Romain Reuillon
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
package mgo

import scala.math._
import scalaz._
import mgo.tools._

object niche {
  type Niche[-I, +T] = (I => T)

  trait Imports {

    def grid(gridSize: Seq[Double])(value: Vector[Double]): Vector[Int] =
      (value zip gridSize).map {
        case (x, g) => (x / g).toInt
      }

    def boundedGrid(lowBound: Vector[Double], highBound: Vector[Double], definition: Vector[Int])(value: Vector[Double]): Vector[Int] =
      (value zip definition zip lowBound zip highBound).map {
        case (((x, d), lb), hb) =>
          val step = (hb - lb) / d
          val p = ((x - lb) / step).floor.toInt
          max(0, min(d, p))
      }

    def irregularGrid(axes: Vector[Vector[Double]])(values: Vector[Double]): Vector[Int] =
      axes zip values map { case (axe, v) => findInterval(axe.sorted, v) }

    def continuousProfile[G](values: G => Vector[Double], x: Int, nX: Int): Niche[G, Int] =
      (genome: G) => {
        val niche = (values(genome)(x) * nX).toInt
        if (niche == nX) niche - 1 else niche
      }

    def boundedContinuousProfile[I](values: I => Vector[Double], x: Int, nX: Int, min: Double, max: Double): Niche[I, Int] =
      (i: I) =>
        values(i)(x) match {
          case v if v < min => -1
          case v if v > max => nX
          case v =>
            val bounded = changeScale(v, min, max, 0, 1)
            val niche = (bounded * nX).toInt
            if (niche == nX) niche - 1 else niche
        }

    def discreteProfile[G](values: G => Vector[Int], x: Int): Niche[G, Int] =
      (genome: G) => values(genome)(x)

    def sequenceNiches[G, T](niches: Vector[Niche[G, T]]): Niche[G, Vector[T]] = { (g: G) => niches.map(_(g)) }

    def mapGenomePlotter[G](x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, Seq[Double]]): Niche[G, (Int, Int)] =
      (genome: G) => {
        val (nicheX, nicheY) = ((values.get(genome)(x) * nX).toInt, (values.get(genome)(y) * nY).toInt)
        (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
      }
  }

}


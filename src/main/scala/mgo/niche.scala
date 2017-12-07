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
      axes zip values map { case (axe, v) => tools.math.findInterval(axe.sorted, v) }

    def genomeProfile[G](values: G => Vector[Double], x: Int, nX: Int): Niche[G, Int] =
      (genome: G) => {
        val niche = (values(genome)(x) * nX).toInt
        if (niche == nX) niche - 1 else niche
      }

    def mapGenomePlotter[G](x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, Seq[Double]]): Niche[G, (Int, Int)] =
      (genome: G) => {
        val (nicheX, nicheY) = ((values.get(genome)(x) * nX).toInt, (values.get(genome)(y) * nY).toInt)
        (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
      }
  }

}


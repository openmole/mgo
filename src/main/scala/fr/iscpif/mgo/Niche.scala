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
package fr.iscpif.mgo

import scalaz._

object niche {
  type Niche[-I, +T] = (I => T)

  def grid[I](gridSize: Seq[Double], grid: I => Seq[Double]): Niche[I, Seq[Int]] =
    (individual: I) => {
      (grid(individual) zip gridSize).map {
        case (x, g) => (x / g).toInt
      }.toArray.toSeq
    }

  def genomeProfile[G](values: G => Vector[Double], x: Int, nX: Int): Niche[G, Int] =
    (genome: G) => {
      val niche = (values(genome)(x) * nX).toInt
      if (niche == nX) niche - 1 else niche
    }

  def mapGenomePlotter[G](x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, Seq[Double] @@ genome.Value]): Niche[G, (Int, Int)] =
    (genome: G) => {
      val (nicheX, nicheY) = ((values.get(genome)(x) * nX).toInt, (values.get(genome)(y) * nY).toInt)
      (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
    }

}

object nicheOld {
  type Niche[-G, -P, +T] = (Individual[G, P] => T)

  def grid[G, P](gridSize: Seq[Double], grid: Individual[G, P] => Seq[Double]) = new Niche[G, P, Seq[Int]] {
    def apply(individual: Individual[G, P]): Seq[Int] =
      (grid(individual) zip gridSize).map {
        case (x, g) => (x / g).toInt
      }.toArray.toSeq
  }

  def genomeProfile[G](x: Int, nX: Int)(implicit values: monocle.Lens[G, Seq[Double] @@ genome.Value]) =
    new Niche[G, Any, Int] {
      override def apply(individual: Individual[G, Any]): Int = {
        val niche = (values.get(individual.genome)(x) * nX).toInt
        if (niche == nX) niche - 1 else niche
      }
    }

  def mapGenomePlotter[G](x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, Seq[Double] @@ genome.Value]) =
    new Niche[G, Any, (Int, Int)] {
      override def apply(i: Individual[G, Any]) = {
        val (nicheX, nicheY) = ((values.get(i.genome)(x) * nX).toInt, (values.get(i.genome)(y) * nY).toInt)
        (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
      }
    }

}


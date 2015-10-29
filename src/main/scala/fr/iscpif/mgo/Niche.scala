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

import fr.iscpif.mgo.Genome._
import monocle._

trait Niche <: Pop {
  trait Niche[+T] <: (Ind => T)
}

trait NicheFunctions <: Niche {

  def grid(gridSize: Seq[Double], grid: Ind => Seq[Double]) = new Niche[Seq[Int]] {
    def apply(individual: Ind): Seq[Int] =
      (grid(individual) zip gridSize).map {
        case (x, g) => (x / g).toInt
      }.toArray.toSeq
  }

  def genomeProfile(x: Int, nX: Int)(implicit values: Lens[G, GenomeValue[Seq[Double]]]) =
    new Niche[Int] {
      override def apply(individual: Ind): Int = {
        val niche = (values.get(individual.genome).value(x) * nX).toInt
        if (niche == nX) niche - 1 else niche
      }
    }

  def mapGenomePlotter (x: Int, nX: Int, y: Int, nY: Int)(implicit values: Lens[G, GenomeValue[Seq[Double]]]) =
    new Niche[(Int, Int)] {
      override def apply(i: Ind) = {
        val (nicheX, nicheY) = ((values.get(i.genome).value(x) * nX).toInt, (values.get(i.genome).value(y) * nY).toInt)
        (if (nicheX == nX) nicheX - 1 else nicheX, if (nicheY == nY) nicheY - 1 else nicheY)
      }
    }

}

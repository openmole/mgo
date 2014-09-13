/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.niche

import fr.iscpif.mgo._
import monocle.syntax._

trait PhenotypeGridNiche <: Niche with DoubleSeqPhenotype {
  type NICHE = Seq[Int]
  def gridSize: Seq[Double]

  def niche(individual: Individual[G, P, F]): Seq[Int] =
    ((individual.phenotype |-> doubleSeq get) zip gridSize).map {
      case (x, g) => (x / g).toInt
    }.toArray.toSeq
}

/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif.mgo.genome

import scala.util.Random

import monocle.Lens

object GAGenome {
  case class Genome(
    values: Array[Double],
    mutation: Option[Int] = None,
    crossover: Option[Int] = None)
}

/**
 * Genome for genetic algorithms
 */
trait GAGenome extends GA {
  type G = GAGenome.Genome

  def rawValues = Lens((_: G).values.toSeq)(v => g => g.copy(values = v.toArray))
  def randomGenome(implicit rng: Random) = GAGenome.Genome(Stream.continually(rng.nextDouble).take(genomeSize).toArray)
}

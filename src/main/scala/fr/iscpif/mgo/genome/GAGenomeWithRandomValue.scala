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

package fr.iscpif.mgo.genome

import monocle.SimpleLens

import scala.util.Random

object GAGenomeWithRandomValue {
  case class Genome(values: Array[Double], randomValues: Array[Double])
}

trait GAGenomeWithRandomValue extends GA with RandomValue {

  type G = GAGenomeWithRandomValue.Genome

  def rawValues = SimpleLens[G, Seq[Double]](_.values.toArray, (g, v) => g.copy(values = v.toArray))
  def randomValues = SimpleLens[G, Seq[Double]](_.randomValues.toArray, (g, v) => g.copy(randomValues = v.toArray))

  def randomGenome(implicit rng: Random) = {
    def rnd = Stream.continually(rng.nextDouble).take(genomeSize).toArray
    GAGenomeWithRandomValue.Genome(rnd, rnd)
  }

}
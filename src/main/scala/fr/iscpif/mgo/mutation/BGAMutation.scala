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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import monocle.syntax._

import scala.util.Random

object BGAMutation {

  def apply(mutation: Mutation with GA)(
    mutationRate: Double = 1.0 / mutation.genomeSize,
    mutationRange: Double = 0.1): mutation.Mutation = {
    import mutation._
    (genome: G, population: Population[G, P, F], archive: A, rng: Random) => {
      val newG = values.get(genome).map {
        g =>
          if (rng.nextDouble < mutationRate) {
            def alphai = if (rng.nextDouble < (1.0 / 16)) 1.0 else 0.0
            def ro = (0 to 15).map { i => alphai * math.pow(2, -i) }.sum
            def sign = if (rng.nextBoolean) 1.0 else -1.0
            g + (sign * mutationRange * ro)
          } else g
      }
      genome |-> values set newG
    }
  }
}


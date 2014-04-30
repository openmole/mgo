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

import scalaz.Lens
import scala.util.Random

object GAGenomeWithSigma {
  case class Genome(values: Seq[Double], sigma: Seq[Double])
}

trait GAGenomeWithSigmaType <: G {
  type G = GAGenomeWithSigma.Genome
}

/**
 * Genome for genetic algorithm with an autoadaptative sigma component
 */
trait GAGenomeWithSigma extends GA with Sigma with GAGenomeWithSigmaType {

  def values = Lens.lensu[G, Seq[Double]]((c, v) => c.copy(values = v), _.values)

  def genome = Lens.lensu[G, Seq[Double]](
    (c, v) =>
      GAGenomeWithSigma.Genome(
        v.slice(0, v.size / 2),
        v.slice(v.size / 2, v.size)
      ),
    v => v.values ++ v.sigma
  )

  def sigma = Lens.lensu[G, Seq[Double]]((c, v) => c.copy(sigma = v), _.sigma)

  def randomGenome(implicit rng: Random) = {
    def rnd = Stream.continually(rng.nextDouble).take(genomeSize).toIndexedSeq
    GAGenomeWithSigma.Genome(rnd, rnd)
  }

}


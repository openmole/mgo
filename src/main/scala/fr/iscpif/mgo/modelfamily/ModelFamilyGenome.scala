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

package fr.iscpif.mgo.modelfamily

import fr.iscpif.mgo.genome.{ GA, GAGenomeWithSigma, Sigma }
import scalaz.Lens
import scala.util.Random

object ModelFamilyGenome {
  case class Genome(modelId: Int, values: Seq[Double], sigma: Seq[Double])
}

trait ModelFamilyGenome <: ModelId with Sigma with GA {

  type G = ModelFamilyGenome.Genome

  def modelId = Lens.lensu[G, Int](
    (c, v) => c.copy(modelId = v),
    _.modelId
  )

  def values = Lens.lensu[G, Seq[Double]]((c, v) => c.copy(values = v), _.values)

  def genome = Lens.lensu[G, Seq[Double]](
    (c, v) =>
      ModelFamilyGenome.Genome(
        v.head.toInt,
        v.tail.slice(0, v.size / 2),
        v.tail.slice(v.size / 2, v.size)
      ),
    v => v.values ++ v.sigma
  )

  def sigma = Lens.lensu[G, Seq[Double]]((c, v) => c.copy(sigma = v), _.sigma)

  def randomGenome(implicit rng: Random) = {
    def rnd = Stream.continually(rng.nextDouble).take(genomeSize).toIndexedSeq
    ModelFamilyGenome.Genome(rng.nextInt, rnd, rnd)
  }
}

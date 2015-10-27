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
package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import Genome._
import Mutation._
import scala.util.Random
import scalaz._

trait GeneticAlgorithm <: Algorithm with MutationFunctions with CrossoverFunctions {

  case class Genome(values: GenomeValue[Seq[Double]], sigma: GenomeSigma, fromMutation: Option[Int] = None, fromCrossover: Option[Int] = None)
  type G = Genome

  implicit val equalsG = Equal.equal[G]((g1, g2) => g1.values == g2.values)
  implicit val genomeValues = monocle.macros.Lenser[G](_.values)
  implicit val genomeSigma = monocle.macros.Lenser[G](_.sigma)

  def randomGenome(size: Int) = State { rng: Random =>
    def genome = Genome(GenomeValue(Seq.fill(size)(rng.nextDouble)), GenomeSigma(Seq.fill(size)(rng.nextDouble)))
    (rng, genome)
  }

  def fromMutation = monocle.macros.Lenser[G](_.fromMutation)
  def fromCrossover = monocle.macros.Lenser[G](_.fromCrossover)

  def mutation =
    dynamicMutation(fromMutation)(
      bga(mutationRate = 1.0 / _, mutationRange = 0.1),
      bga(mutationRate = _ => 0.5, mutationRange = 0.5),
      adaptiveCauchy()
    )

  def crossover =
    dynamicCrossover(fromCrossover)(blx(0.5), sbx(0.1))

}


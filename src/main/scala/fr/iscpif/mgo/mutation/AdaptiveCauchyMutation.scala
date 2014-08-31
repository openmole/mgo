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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import tools._
import org.apache.commons.math3.distribution.CauchyDistribution
import util.Random
import scala.math._

/**
 * Mutation of a genome based on gausian distribution arrount the genome with
 * adaptive sigma values.
 * See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
 * See on paper : Gaussian mutation and self adaptation (Hinterding) &&
 * Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert
 * Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It,
 * Modern Heuristics
 */
object AdaptiveCauchyMutation {

  def mutate(genome: Seq[Double], sigma: Seq[Double], minimumSigma: Double)(implicit rng: Random) = {
    val indexedSeqSigma = sigma.map { s => math.max(minimumSigma, s * exp(rng.nextGaussian)) }

    val newValues =
      (genome zip indexedSeqSigma) map {
        case (v, s) => new CauchyDistribution(rng, v, s).sample //.nextGaussian * s + v
      }

    (newValues, indexedSeqSigma)
  }

}

trait AdaptiveCauchyMutation <: Mutation with Sigma with GA with MinimumSigma {

  override def mutate(genome: G, population: Population[G, P, F], archive: A)(implicit rng: Random): G = {
    val (newValues, indexedSeqSigma) = AdaptiveCauchyMutation.mutate(values.get(genome), sigma.get(genome), minimumSigma)
    newValues.foreach(v => assert(!v.isNaN))
    val updatedValues = values.set(genome, newValues)
    sigma.set(updatedValues, indexedSeqSigma)
  }

}
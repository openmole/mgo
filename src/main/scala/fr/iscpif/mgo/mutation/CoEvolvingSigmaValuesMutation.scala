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
import fr.iscpif.mgo.tools.Math._
import fr.iscpif.mgo.tools.Random._
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
trait CoEvolvingSigmaValuesMutation extends Mutation with Sigma with GA {

  override def mutate(genome: G, population: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random): G = {
    val indexedSeqSigma = sigma.get(genome).map { s => clamp(s * exp(rng.nextGaussian), 0, 1) }

    val newValues =
      (values.get(genome) zip indexedSeqSigma) map {
        case (v, s) => clamp(rng.nextGaussian * s + v, 0, 1)
      }

    val updatedValues = values.set(genome, newValues)
    sigma.set(updatedValues, indexedSeqSigma)
  }

}
/*
 * Copyright (C) 2011 Sebastien Rey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.GenomeFactory
import java.util.Random
import scala.math._

/**
 * Polynomial mutationolynomial mutation by Deb and Goyal. If is the value of
 * the ith parameter selected for mutation with a probability pm and the result
 * of the mutation is the new value obtained by a polynomial probability
 * distribution.
 * Based on the source code of Jmetal library
 * Author : Antonio J. Nebro <antonio@lcc.uma.es> and Juan J. Durillo <durillo@lcc.uma.es>
 */
trait PolynomialMutation extends Mutation with GenomeFactory {

  type G <: GAGenome

  def mutationRate = 0.5

  def distributionIndex: Double

  override def mutate(genome: G)(implicit aprng: Random): G = {
    val newValues = genome.values map {
      v =>
        if (aprng.nextDouble <= mutationRate) {
          val yl = 0.0 // lower bound
          val yu = 1.0 // upper bound
          val delta1 = (v - yl) / (yu - yl)
          val delta2 = (yu - v) / (yu - yl)
          val mut_pow = 1.0 / (distributionIndex + 1.0)
          val rnd = aprng.nextDouble

          val deltaq: Double = (if (rnd <= 0.5) {
            val xy = 1.0 - delta1
            val value = 2.0 * rnd + (1.0 - 2.0 * rnd) * (pow(xy, (distributionIndex + 1.0)))
            pow(value, mut_pow) - 1.0
          } else {
            val xy = 1.0 - delta2
            val value = 2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * (pow(xy, (distributionIndex + 1.0)))
            1.0 - (pow(value, mut_pow))
          })

          val finalValue = v + deltaq * (yu - yl)

          if (finalValue < yl) yl
          else if (finalValue > yu) yu
          else finalValue
        }
        v
    }
    genomeFactory(genome.updatedValues(newValues))
  }
}

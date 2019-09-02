/*
 * Copyright (C) 2018-02-22 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mgo.tools

import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.stat.correlation.Covariance
import scala.math._
import scala.util.Random
import scala.collection.Searching.search

object stats {

  def weightedCovariance(data: RealMatrix, weights: Array[Double]): RealMatrix = {
    val n = data.getRowDimension()
    val weightsSum = weights.sum
    val weightsSumSquared = pow(weightsSum, 2)
    val weightsSquaredSum = weights.map { pow(_, 2) }.sum
    val dataMean = MatrixUtils.createRealVector(
      data.transpose().operate(weights)).mapDivide(weightsSum)
    val dataCenteredWeighted = (0 to n - 1).map { i =>
      data.getRowVector(i).subtract(dataMean).mapMultiply(sqrt(weights(i)))
        .toArray
    }.toArray
    val cov = new Covariance(dataCenteredWeighted, false)
      .getCovarianceMatrix()
      .scalarMultiply(n.toDouble / weightsSum)
      // Compute the unbiased weighted variance
      .scalarMultiply(weightsSumSquared /
        (weightsSumSquared - weightsSquaredSum))
    cov
  }

  def weightedSample[T](n: Int, data: Vector[T], weights: Vector[Double])(implicit rng: Random): Vector[T] = {
    val cumul = weights.drop(1).scanLeft(weights(0)) {
      case (acc, x) => acc + x
    }
    val randoms = Vector.fill(n)(() => rng.nextDouble()).map(_())
    randoms.map { r => data(cumul.search(r).insertionPoint) }
  }
}


/*
 * Copyright (C) 10/07/2017 Guillaume Chérel
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
package mgo.algorithm.sensitivityAnalysis

import sensitivityAnalysis.SATotalOrder
import mgo.sampling.{ lhs, replaceColumnsOneByOne }
import util.Random

/** This package provides functions to build sensitivity analyses
  * (SATotalOrder) as described by Saltelli (see Saltelli et al. 2010
  * Variance based sensitivity analysis of model output. Design and
  * estimator for the total sensitivity index.) */
object globalSensitivityAnalysis {

  /** Total order effect sensitivity analysis of a function from R^n to
    * R where inputs are sampled uniformly within the subspace delimited
    * by the given bounds. */
  def uniformInputs_Rn_R(minmax: Vector[(Double, Double)], numberOfSamples: Int)(implicit rng: Random) =
    SATotalOrder[Vector[Double], Double](
      run = f => {

        /**
         * Scale each column of the given matrix which values are assumed
         * between 0 and 1 to the interval bounded by min and max.
         */
        def scale(mat: Vector[Vector[Double]]) =
          (mat.transpose zip minmax).map {
            case (column, (min, max)) =>
              column.map { _ * (max - min) + min }
          }.transpose

        val matA = scale(lhs(minmax.size, numberOfSamples))
        val matB = scale(lhs(minmax.size, numberOfSamples))

        totalOrderIndex(
          matB.map(f.andThen(Option.apply)),
          replaceColumnsOneByOne(matB, matA).map(_.map(f.andThen(Option.apply)))
        )
      }
    )

  /**
   * Total order effect sensitivity analysis when the output values of
   * the function being analysed are already known and can be provided.
   * fB(j) contains the model output value for the j-th row of B,
   * f(B_j). fC(i)(j) contains the model output for the j-th row of matrix
   * C^i (the matrix where all columns are from B except the i-th which
   * is from A).  Rows of matrices A and B represent individual inputs
   * to the function f and the columns are sampled uniformly.
   */
  def fromPrecomputed_Rn_R(fB: Vector[Option[Double]], fC: Vector[Vector[Option[Double]]]): SATotalOrder[Unit, Unit] =
    SATotalOrder(
      run = _ => totalOrderIndex(fB, fC)
    )

  /**
   * Compute the total order effect from the given output values
   * of a model. From Saltelli 2008 Global Sensitivity Analysis.
   * fA(j) contains the model output value for the j-th row of A,
   * f(A_j). fC(i)(j) contains the model output for the j-th row of matrix
   * C^i (the matrix where all columns are from B except the i-th which
   * is from A).
   */
  def totalOrderIndex(fB: Vector[Option[Double]], fC: Vector[Vector[Option[Double]]]): Vector[Double] = {
    val fBSome = fB.collect { case Some(i) => i }
    val NB = fBSome.size
    val k = fC.size //Number of parameters
    val f02 = math.pow(fBSome.sum / NB.toDouble, 2)
    val varY = fBSome.map(fBj => math.pow(fBj, 2)).sum / NB.toDouble - f02
    def avgProduct(fB: Vector[Option[Double]], fCi: Vector[Option[Double]]): Double = {
      val prods = (fB zip fCi).collect({ case (Some(fBj), Some(fCij)) => fBj * fCij })
      prods.sum / prods.size.toDouble
    }

    (1 to k).map { i =>
      {
        val innerJoin = (fB zip fC(i - 1)).collect { case (Some(fBj), Some(fCij)) => (fBj, fCij) }
        val N = innerJoin.size
        val sumSquaredDiff = innerJoin.map { case (fBj, fCij) => math.pow(fBj - fCij, 2) }.sum
        (sumSquaredDiff / (2.0 * innerJoin.size)) / varY
      }
    }.toVector
  }

}

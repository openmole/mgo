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

package fr.iscpif.mgo.test

import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.optim._
import org.apache.commons.math3.optim.nonlinear.scalar.{ GoalType, ObjectiveFunction }
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.CMAESOptimizer
import org.apache.commons.math3.optim.univariate.SearchInterval
import org.apache.commons.math3.random.Well1024a
import org.apache.commons.math3.util.{ FastMath, MathArrays }

object TestCMAES extends App {

  val rng = new Well1024a

  val f = new MultivariateFunction {
    override def value(point: Array[Double]): Double = {
      point.max
    }
  }

  val cc = new ConvergenceChecker[PointValuePair] {
    override def converged(iteration: Int, previous: PointValuePair, current: PointValuePair): Boolean = false
  }

  val initialGuess = Iterator.continually(rng.nextDouble).take(2).toArray
  val initialSigma = Iterator.continually(rng.nextDouble).take(2).toArray
  val lowerBounds = Iterator.continually(0.0).take(2).toArray
  val upperBounds = Iterator.continually(1.0).take(2).toArray

  val optimizer = new CMAESOptimizer(
    10, -1, true, 0, 10, rng, false, cc)

  val solution = optimizer.optimize(
    new ObjectiveFunction(f),
    GoalType.MINIMIZE,
    //new SearchInterval(0, 1),
    new InitialGuess(initialGuess),
    new CMAESOptimizer.PopulationSize(100),
    new CMAESOptimizer.Sigma(initialSigma),
    new SimpleBounds(lowerBounds, upperBounds),
    new MaxEval(Int.MaxValue),
    new MaxIter(Int.MaxValue))

  println(solution.getPoint.toSeq)

  /*

  def optim = {
    // Generate and evaluate lambda offspring
    val arz: RealMatrix = randn1(dimension, lambda)
    val arx: RealMatrix = zeros(dimension, lambda)

    val fitness: Array[Double] = new Array[Double](lambda)

    val valuePenaltyPairs: Array[CMAESOptimizer.ValuePenaltyPair] = new Array[CMAESOptimizer.ValuePenaltyPair](lambda)


    // generate random offspring
    {
      var k: Int = 0
      while (k < lambda) {
        {
          var arxk: RealMatrix = null
          {
            var i: Int = 0
            while (i < checkFeasableCount + 1) {
              {
                if (diagonalOnly <= 0) {
                  arxk = xmean.add(BD.multiply(arz.getColumnMatrix(k)).scalarMultiply(sigma))
                }
                else {
                  arxk = xmean.add(times(diagD, arz.getColumnMatrix(k)).scalarMultiply(sigma))
                }
                if (i >= checkFeasableCount || fitfun.isFeasible(arxk.getColumn(0))) {
                  break //todo: break is not supported
                }
                arz.setColumn(k, randn(dimension))
              }
              ({
                i += 1; i - 1
              })
            }
          }
          copyColumn(arxk, 0, arx, k)
          valuePenaltyPairs(k) = fitfun.value(arx.getColumn(k))
        }
        ({
          k += 1; k - 1
        })
      }
    }



    // Compute fitnesses by adding value and penalty after scaling by value range.
    val valueRange: Double = valueRange(valuePenaltyPairs)
    {
      var iValue: Int = 0
      while (iValue < valuePenaltyPairs.length) {
        {
          fitness(iValue) = valuePenaltyPairs(iValue).value + valuePenaltyPairs(iValue).penalty * valueRange
        }
        ({
          iValue += 1; iValue - 1
        })
      }
    }



    // Sort by fitness and compute weighted mean into xmean
    val arindex: Array[Int] = sortedIndices(fitness)


    // Calculate new xmean, this is selection and recombination
    val xold: RealMatrix = xmean


    val bestArx: RealMatrix = selectColumns(arx, MathArrays.copyOf(arindex, mu))


    xmean = bestArx.multiply(weights)


    val bestArz: RealMatrix = selectColumns(arz, MathArrays.copyOf(arindex, mu))


    val zmean: RealMatrix = bestArz.multiply(weights)


    val hsig: Boolean = updateEvolutionPaths(zmean, xold)


    if (diagonalOnly <= 0) {
      updateCovariance(hsig, bestArx, arz, arindex, xold)
    }
    else {
      updateCovarianceDiagonalOnly(hsig, bestArz)
    }


    // Adapt step size sigma - Eq. (5)


    sigma *= FastMath.exp(FastMath.min(1, (normps / chiN - 1) * cs / damps))


    val bestFitness: Double = fitness(arindex(0))


    val worstFitness: Double = fitness(arindex(arindex.length - 1))


    if (bestValue > bestFitness) {
      bestValue = bestFitness
      lastResult = optimum
      optimum = new PointValuePair(fitfun.repair(bestArx.getColumn(0)), if (isMinimize) bestFitness else -bestFitness)
      if (getConvergenceChecker != null && lastResult != null && getConvergenceChecker.converged(iterations, optimum, lastResult)) {
        break //todo: label break is not supported
      }
    }


    // handle termination criteria
    // Break, if fitness is good enough


    if (stopFitness != 0 && bestFitness < (if (isMinimize) stopFitness else -stopFitness)) {
      break //todo: label break is not supported
    }


    val sqrtDiagC: Array[Double] = sqrt(diagC).getColumn(0)


    val pcCol: Array[Double] = pc.getColumn(0)
    {
      var i: Int = 0
      while (i < dimension) {
        {
          if (sigma * FastMath.max(FastMath.abs(pcCol(i)), sqrtDiagC(i)) > stopTolX) {
            break //todo: break is not supported
          }
          if (i >= dimension - 1) {
            break //todo: label break is not supported
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
    {
      var i: Int = 0
      while (i < dimension) {
        {
          if (sigma * sqrtDiagC(i) > stopTolUpX) {
            break //todo: label break is not supported
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }


    val historyBest: Double = min(fitnessHistory)


    val historyWorst: Double = max(fitnessHistory)


    if (iterations > 2 && FastMath.max(historyWorst, worstFitness) - FastMath.min(historyBest, bestFitness) < stopTolFun) {
      break //todo: label break is not supported
    }


    if (iterations > fitnessHistory.length && historyWorst - historyBest < stopTolHistFun) {
      break //todo: label break is not supported
    }


    // condition number of the covariance matrix exceeds 1e14


    if (max(diagD) / min(diagD) > 1e7) {
      break //todo: label break is not supported
    }


    // user defined termination


    if (getConvergenceChecker != null) {
      val current: PointValuePair = new PointValuePair(bestArx.getColumn(0), if (isMinimize) bestFitness else -bestFitness)
      if (lastResult != null && getConvergenceChecker.converged(iterations, current, lastResult)) {
        break //todo: label break is not supported
      }
      lastResult = current
    }


    // Adjust step size in case of equal function values (flat fitness)


    if (bestValue == fitness(arindex((0.1 + lambda / 4.).asInstanceOf[Int]))) {
      sigma *= FastMath.exp(0.2 + cs / damps)
    }


    if (iterations > 2 && FastMath.max(historyWorst, bestFitness) - FastMath.min(historyBest, bestFitness) == 0) {
      sigma *= FastMath.exp(0.2 + cs / damps)
    }
  }
*/
}

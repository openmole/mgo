/*
 * Copyright (C) 29/01/2018 Guillaume Chérel
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

package mgo.abc

import org.apache.commons.math3.analysis.function.Sqrt
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.linear.RealVector
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import org.apache.commons.math3.stat.StatUtils
import mgo.tools.stats.weightedCovariance
import mgo.tools.stats.weightedSample
import mgo.tools.execution._
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.SortedSet
import scala.math._

/**
 * Adaptive Population Monte Carlo approximate Bayesian
 * computation. M. Lenormand, F. Jabot, G. Deffuant; Adaptive approximate
 * Bayesian computation for complex models. 2012.
 */
object APMC {

  case class Params(
    n: Int,
    nAlpha: Int,
    pAccMin: Double,
    priorSample: () => Array[Double],
    priorDensity: Array[Double] => Double,
    observed: Array[Double])

  case class State(
    thetas: RealMatrix,
    weights: Array[Double],
    rhos: RealVector,
    pAcc: Double,
    epsilon: Double)

  def run(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): State = exposedEval(p).run(f)

  def scan(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): Vector[State] = exposedEval(p).scan(f)

  def exposedEval(p: Params)(implicit rng: RandomGenerator): ExposedEval[State, RealMatrix, (State, State, RealMatrix, RealMatrix), Vector[Double], Vector[Double]] =
    ExposedEval(
      initPreEval = { () =>
        val thetasRM = initPreEval(p)
        val thetasV = thetasRM.getData().toVector.map { _.toVector }
        (thetasRM, thetasV)
      },
      initPostEval = { (thetas, xsV) =>
        val xs = MatrixUtils.createRealMatrix(xsV.toArray.map { _.toArray })
        initPostEval(p, thetas, xs)
      },
      stepPreEval = { s =>
        val (sPreEval, sigmaSquared, newThetas) = stepGenPreEval(p, s)
        val newThetasV = newThetas.getData().toVector.map { _.toVector }
        ((s, sPreEval, sigmaSquared, newThetas), newThetasV)
      },
      stepPostEval = { (sstep, newXsV) =>
        val (s, sPreEval, sigmaSquared, newThetas) = sstep
        val newXs = MatrixUtils.createRealMatrix(
          newXsV.toArray.map { _.toArray })
        val newS = stepGenPostEval(p, sPreEval, sigmaSquared, newThetas, newXs)
        stepMerge(p, s, newS)
      },
      stop = stop(p, _))

  def stop(p: Params, s: State): Boolean = s.pAcc <= p.pAccMin

  def init(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): State = {
    val thetas = initPreEval(p)
    val xs = MatrixUtils.createRealMatrix(
      Array.tabulate(p.n) { i => f(thetas.getRow(i).toVector).toArray })
    val state = initPostEval(p, thetas, xs)
    state
  }

  def initPreEval(p: Params)(implicit rng: RandomGenerator): RealMatrix = {
    val thetas = MatrixUtils.createRealMatrix(Array.fill(p.n)(p.priorSample()))
    thetas
  }

  def initPostEval(p: Params, thetas: RealMatrix, xs: RealMatrix)(implicit rng: RandomGenerator): State = {
    val dim = thetas.getColumnDimension()
    val obs = MatrixUtils.createRealVector(p.observed)
    val rhos = MatrixUtils.createRealVector(
      Array.tabulate(p.n) { i =>
        xs.getRowVector(i).getDistance(obs)
      })
    val (thetasSelected, rhosSelected, epsilon) =
      filterParticles(p.nAlpha, thetas, rhos)
    val weightsSelected = Array.fill(p.nAlpha)(1.0)
    State(
      thetas = thetasSelected,
      weights = weightsSelected,
      rhos = rhosSelected,
      pAcc = 1,
      epsilon = epsilon)
  }

  def step(p: Params, f: Vector[Double] => Vector[Double], s: State)(implicit rng: RandomGenerator): State = stepMerge(p, s, stepGen(p, f, s))

  def stepGen(p: Params, f: Vector[Double] => Vector[Double], s: State)(implicit rng: RandomGenerator): State = {
    val (state1, sigmaSquared, newThetas) = stepGenPreEval(p, s)
    val newXs = MatrixUtils.createRealMatrix(
      Array.tabulate(p.n - p.nAlpha) { i => f(newThetas.getRow(i).toVector).toArray })
    val finalState = stepGenPostEval(p, state1, sigmaSquared, newThetas, newXs)
    finalState
  }

  def stepGenPreEval(p: Params, s: State)(implicit rng: RandomGenerator): (State, RealMatrix, RealMatrix) = {
    val dim = s.thetas.getColumnDimension()
    val sigmaSquared = weightedCovariance(s.thetas, s.weights)
      .scalarMultiply(2)
    val weightedDistributionTheta = new EnumeratedIntegerDistribution(
      rng, Array.range(0, p.nAlpha), s.weights)
    val newThetas = MatrixUtils.createRealMatrix(
      Array.fill(p.n - p.nAlpha) {
        val resampledTheta = s.thetas.getRow(weightedDistributionTheta.sample)
        new MultivariateNormalDistribution(
          rng, resampledTheta, sigmaSquared.getData).sample
      })

    (s, sigmaSquared, newThetas)
  }

  def stepGenPostEval(
    p: Params,
    s: State,
    sigmaSquared: RealMatrix,
    newThetas: RealMatrix,
    newXs: RealMatrix): State = {
    val obs = MatrixUtils.createRealVector(p.observed)
    val newRhos = MatrixUtils.createRealVector(
      Array.tabulate(p.n - p.nAlpha) { i =>
        newXs.getRowVector(i).getDistance(obs)
      })
    val newPAcc = newRhos.toArray.count { r => r < s.epsilon }.toDouble /
      (p.n - p.nAlpha).toDouble
    val (thetasSelected, rhosSelected, newEpsilon) =
      filterParticles(p.nAlpha, newThetas, newRhos)
    val weightsSelected = compWeights(p, s, sigmaSquared, thetasSelected)
    State(
      thetas = thetasSelected,
      weights = weightsSelected,
      rhos = rhosSelected,
      pAcc = newPAcc,
      epsilon = s.epsilon)
  }

  def stepMerge(p: Params, s1: State, s2: State): State = {
    val allRhos = MatrixUtils.createRealVector(
      s1.rhos.toArray ++ s2.rhos.toArray)
    val select = allRhos.toArray.zipWithIndex
      .sortBy { _._1 }.take(p.nAlpha).map { _._2 }
    val newEpsilon = allRhos.getEntry(select.last)
    val ns1 = s1.thetas.getRowDimension()
    val thetasSelected = MatrixUtils.createRealMatrix(
      select.map { i =>
        if (i < ns1) { s1.thetas.getRow(i) }
        else { s2.thetas.getRow(i - ns1) }
      })
    val rhosSelected = MatrixUtils.createRealVector(
      select.map { i =>
        if (i < ns1) { s1.rhos.getEntry(i) }
        else { s2.rhos.getEntry(i - ns1) }
      })
    val weightsSelected =
      select.map { i =>
        if (i < ns1) { s1.weights(i) }
        else { s2.weights(i - ns1) }
      }
    State(
      thetas = thetasSelected,
      weights = weightsSelected,
      rhos = rhosSelected,
      pAcc = s2.pAcc,
      epsilon = newEpsilon)
  }

  def filterParticles(
    keep: Int,
    thetas: RealMatrix,
    rhos: RealVector): (RealMatrix, RealVector, Double) = {
    val dim = thetas.getColumnDimension()
    val select = rhos.toArray().zipWithIndex.sortBy { _._1 }.take(keep).map { _._2 }.toArray
    val newEpsilon = rhos.getEntry(select.last)
    val thetasSelected = thetas.getSubMatrix(select, (0 until dim).toArray)
    val rhosSelected = MatrixUtils.createRealVector(
      select.map { i => rhos.getEntry(i) })

    (thetasSelected, rhosSelected, newEpsilon)
  }

  def compWeights(p: Params, s: State, sigmaSquared: RealMatrix, thetasSelected: RealMatrix): Array[Double] = {
    val weightsSum = s.weights.sum
    val sqrtDet2PiSigmaSquared = sqrt(abs(new LUDecomposition(
      sigmaSquared.scalarMultiply(2.0 * Pi)).getDeterminant()))
    val inverseSigmaSquared = new LUDecomposition(sigmaSquared).getSolver()
      .getInverse()
    val weightsSelected =
      (0 until thetasSelected.getRowDimension()).map { i =>
        val thetaI = thetasSelected.getRowVector(i)
        p.priorDensity(thetaI.toArray) /
          (0 until p.nAlpha).map { j =>
            val weightJ = s.weights(j)
            val thetaJ = s.thetas.getRowVector(j)
            val thetaDiff = MatrixUtils.createRowRealMatrix(
              thetaI.subtract(thetaJ).toArray())
            (weightJ / weightsSum) *
              exp(
                -0.5 * thetaDiff.multiply(inverseSigmaSquared)
                  .multiply(thetaDiff.transpose).getEntry(0, 0)) /
                sqrtDet2PiSigmaSquared
          }.sum
      }.toArray
    weightsSelected
  }
}


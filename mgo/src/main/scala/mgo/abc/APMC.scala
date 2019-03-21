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
import mgo.tools.LinearAlgebra.functorVectorVectorDoubleToRealMatrix
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
    t0: Int,
    t: Int,
    ts: Vector[Int],
    weights: Array[Double],
    rhos: RealVector,
    pAcc: Double,
    epsilon: Double)

  def sequential(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): Sequential[State] = Sequential[State](() => init(p, f), step(p, f, _), stop(p, _))

  def run(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): State = sequential(p, f).run

  def scan(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): Vector[State] = sequential(p, f).scan

  def stop(p: Params, s: State): Boolean = s.pAcc <= p.pAccMin

  def init(p: Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): State = {
    exposedInit(p).run(functorVectorVectorDoubleToRealMatrix(_.map { f }))(())
  }

  def exposedInit(p: Params)(implicit rng: RandomGenerator): ExposedEval[Unit, RealMatrix, RealMatrix, RealMatrix, State] =
    ExposedEval(
      pre = { _: Unit =>
        val thetas = initPreEval(p)
        (thetas, thetas)
      },
      post = { (thetas, xs) =>
        initPostEval(p, thetas, xs)
      })

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
    val (rhosSelected, select) =
      rhos.toArray().zipWithIndex
        .sortBy { _._1 }
        .take(p.nAlpha)
        .unzip
    val epsilon = rhosSelected.last
    val thetasSelected = thetas.getSubMatrix(select, (0 until dim).toArray)
    val t = 1
    val tsSelected = Vector.fill(p.nAlpha)(t)
    val weightsSelected = Array.fill(p.nAlpha)(1.0)
    State(
      thetas = thetasSelected,
      t0 = 0,
      t = t,
      ts = tsSelected,
      weights = weightsSelected,
      rhos = MatrixUtils.createRealVector(rhosSelected),
      pAcc = 1,
      epsilon = epsilon)
  }

  def step(p: Params, f: Vector[Double] => Vector[Double], s: State)(implicit rng: RandomGenerator): State =
    exposedStep(p).run(functorVectorVectorDoubleToRealMatrix(_.map { f }))(s)

  def exposedStep(p: Params)(implicit rng: RandomGenerator): ExposedEval[State, RealMatrix, (State, RealMatrix, RealMatrix), RealMatrix, State] =
    ExposedEval(
      pre = { s =>
        val (sigmaSquared, newThetas) = stepPreEval(p, s)
        ((s, sigmaSquared, newThetas), newThetas)
      },
      post = { (sstep, newXs) =>
        val (s, sigmaSquared, newThetas) = sstep
        stepPostEval(p, s, sigmaSquared, newThetas, newXs)
      })

  def stepPreEval(p: Params, s: State)(implicit rng: RandomGenerator): (RealMatrix, RealMatrix) = {
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

    (sigmaSquared, newThetas)
  }

  def stepPostEval(
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
    val (thetasSelected, rhosSelected, weightsSelected, tsSelected, newThetasSelected, newRhosSelected, newEpsilon) =
      filterParticles(p.nAlpha, s.thetas, s.rhos, s.weights, s.ts,
        newThetas, newRhos)
    val newPAcc = newRhosSelected.getDimension().toDouble / (p.n - p.nAlpha).toDouble
    val newT = s.t + 1
    val newTsSelected = Vector.fill(newThetasSelected.getRowDimension())(newT)
    val newWeightsSelected = compWeights(p, s, sigmaSquared, newThetasSelected)
    State(
      t0 = s.t0,
      t = newT,
      thetas = MatrixUtils.createRealMatrix(
        thetasSelected.getData() ++ newThetasSelected.getData()),
      rhos = MatrixUtils.createRealVector(
        rhosSelected.toArray() ++ newRhosSelected.toArray()),
      weights = weightsSelected ++ newWeightsSelected,
      ts = tsSelected ++ newTsSelected,
      pAcc = newPAcc,
      epsilon = newEpsilon)
  }

  def filterParticles(
    keep: Int,
    thetas: RealMatrix,
    rhos: RealVector,
    weights: Array[Double],
    ts: Vector[Int],
    newThetas: RealMatrix,
    newRhos: RealVector): (RealMatrix, RealVector, Array[Double], Vector[Int], RealMatrix, RealVector, Double) = {
    val dim = thetas.getColumnDimension()
    val select_ =
      (rhos.toArray().zipWithIndex.map { case (r, i) => (r, 1, i) } ++
        newRhos.toArray().zipWithIndex.map { case (r, i) => (r, 2, i) })
        .sortBy { _._1 }
        .take(keep)
        .partition { _._2 == 1 }
    val (rhosSelected, _, select) = select_._1.unzip3
    val (newRhosSelected, _, newSelect) = select_._2.unzip3
    val newEpsilon = max(rhosSelected.last, newRhosSelected.last)
    val thetasSelected = thetas.getSubMatrix(select, (0 until dim).toArray)
    val newThetasSelected = newThetas.getSubMatrix(newSelect, (0 until dim).toArray)
    val weightsSelected = select.map { weights(_) }.toArray
    val tsSelected = select.map { ts(_) }.toVector

    (
      thetasSelected,
      MatrixUtils.createRealVector(rhosSelected),
      weightsSelected,
      tsSelected,
      newThetasSelected,
      MatrixUtils.createRealVector(newRhosSelected),
      newEpsilon)
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


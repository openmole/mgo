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

import mgo.tools.LinearAlgebra._
import mgo.tools.execution._
import mgo.tools._
import mgo.tools.stats.weightedCovariance
import org.apache.commons.math3.distribution.{ EnumeratedIntegerDistribution, MultivariateNormalDistribution }
import org.apache.commons.math3.linear.{ LUDecomposition, MatrixUtils, RealMatrix, RealVector }

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
    priorSample: util.Random => Array[Double],
    priorDensity: Array[Double] => Double,
    observed: Array[Double])

  case class State(
    thetas: Matrix,
    t: Int,
    ts: Vector[Int],
    weights: Array[Double],
    rhos: Array[Double],
    pAcc: Double,
    epsilon: Double)

  def sequential(p: Params, f: (Vector[Double], util.Random) => Vector[Double])(implicit rng: util.Random): Sequential[State] = Sequential[State](() => init(p.n, p.nAlpha, p.observed, p.priorSample, f), step(p, f, _), stop(p.pAccMin, _))

  def run(p: Params, f: (Vector[Double], util.Random) => Vector[Double])(implicit rng: util.Random): State = sequential(p, f).run

  def scan(p: Params, f: (Vector[Double], util.Random) => Vector[Double])(implicit rng: util.Random): Vector[State] = sequential(p, f).scan

  def stop(pAccMin: Double, s: State): Boolean = s.pAcc <= pAccMin

  def init(n: Int, nAlpha: Int, observed: Array[Double], priorSample: util.Random => Array[Double], f: (Vector[Double], util.Random) => Vector[Double])(implicit rng: util.Random): State =
    exposedInit(n, nAlpha, observed, priorSample).run(functorVectorVectorDoubleToMatrix(_.map { f(_, rng) }))(())

  def exposedInit(n: Int, nAlpha: Int, observed: Array[Double], priorSample: util.Random => Array[Double])(implicit rng: util.Random): ExposedEval[Unit, Matrix, Matrix, Matrix, State] =
    ExposedEval(
      pre = (_: Unit) => {
        val thetas = initPreEval(n, priorSample)
        (thetas, thetas)
      },
      post = (thetas, xs) => { initPostEval(n, nAlpha, observed, thetas, xs) })

  def initPreEval(n: Int, priorSample: util.Random => Array[Double])(implicit rng: util.Random): Matrix = Array.fill(n)(priorSample(rng))

  def initPostEval(n: Int, nAlpha: Int, observed: Array[Double], thetas: Matrix, xs: Matrix)(implicit rng: util.Random): State = {
    val thetasM = MatrixUtils.createRealMatrix(thetas)
    val xsM = MatrixUtils.createRealMatrix(xs)

    val dim = thetasM.getColumnDimension()
    val obs = MatrixUtils.createRealVector(observed)
    val rhos = MatrixUtils.createRealVector(Array.tabulate(n) { i => xsM.getRowVector(i).getDistance(obs) })

    val (rhosSelected, select) =
      rhos.toArray().zipWithIndex
        .sortBy { _._1 }
        .take(nAlpha)
        .unzip
    val epsilon = rhosSelected.last
    val thetasSelected = thetasM.getSubMatrix(select, (0 until dim).toArray)
    val t = 1
    val tsSelected = Vector.fill(rhosSelected.size)(t)
    val weightsSelected = Array.fill(rhosSelected.size)(1.0)
    State(
      thetas = thetasSelected.getData,
      t = t,
      ts = tsSelected,
      weights = weightsSelected,
      rhos = rhosSelected,
      pAcc = 1,
      epsilon = epsilon)
  }

  def step(p: Params, f: (Vector[Double], util.Random) => Vector[Double], s: State)(implicit rng: util.Random): State =
    exposedStep(p).run(functorVectorVectorDoubleToMatrix(_.map { f(_, rng) }))(s)

  def exposedStep(p: Params)(implicit rng: util.Random): ExposedEval[State, Matrix, (State, Matrix, Matrix), Matrix, State] =
    ExposedEval(
      pre = { s =>
        val (sigmaSquared, newThetas) = stepPreEval(p.n, p.nAlpha, s)
        ((s, sigmaSquared, newThetas), newThetas)
      },
      post = { (sstep, newXs) =>
        val (s, sigmaSquared, newThetas) = sstep
        stepPostEval(p.n, p.nAlpha, p.priorDensity, p.observed, s, sigmaSquared, newThetas, newXs)
      })

  def stepPreEval(n: Int, nAlpha: Int, s: State)(implicit rng: util.Random): (Matrix, Matrix) = {
    val thetasM = MatrixUtils.createRealMatrix(s.thetas)

    val dim = thetasM.getColumnDimension()
    val sigmaSquared = weightedCovariance(thetasM, s.weights).scalarMultiply(2)

    val weightedDistributionTheta = new EnumeratedIntegerDistribution(apacheRandom(rng), Array.range(0, nAlpha), s.weights)
    val newThetas =
      Array.fill(n - nAlpha) {
        val resampledTheta = thetasM.getRow(weightedDistributionTheta.sample)
        new MultivariateNormalDistribution(apacheRandom(rng), resampledTheta, sigmaSquared.getData).sample
      }

    (sigmaSquared.getData, newThetas)
  }

  def stepPostEval(
    n: Int,
    nAlpha: Int,
    priorDensity: Array[Double] => Double,
    observed: Array[Double],
    s: State,
    sigmaSquared: Matrix,
    newThetas: Matrix,
    newXs: Matrix): State = {

    val thetasM = MatrixUtils.createRealMatrix(s.thetas)
    val newThetasM = MatrixUtils.createRealMatrix(newThetas)
    val newXsM = MatrixUtils.createRealMatrix(newXs)
    val rhosV = MatrixUtils.createRealVector(s.rhos)
    val sigmaSquaredM = MatrixUtils.createRealMatrix(sigmaSquared)

    val obs = MatrixUtils.createRealVector(observed)
    val newRhos = MatrixUtils.createRealVector(
      Array.tabulate(n - nAlpha) { i => newXsM.getRowVector(i).getDistance(obs) })

    val (resSelected, resNewSelected, resNewEpsilon) =
      filterParticles(nAlpha, thetasM, rhosV, s.weights, s.ts, newThetasM, newRhos)

    val (thetasSelected, rhosSelected, weightsSelected, tsSelected) =
      resSelected match {
        case None => (Array.empty, Array.empty, Array.empty, Vector.empty)
        case Some((th, r, w, ts)) => (th.getData(), r.toArray(), w, ts)
      }

    val (newThetasSelected, newRhosSelected) =
      resNewSelected match {
        case None => (Array.empty[Array[Double]], Array.empty)
        case Some((th, r)) => (th.getData(), r.toArray())
      }

    val newWeightsSelected = compWeights(nAlpha, priorDensity, s, sigmaSquaredM, newThetasSelected)

    val newEpsilon: Double = resNewEpsilon.getOrElse(0)
    val newPAcc = newRhosSelected.size.toDouble / (n - nAlpha).toDouble
    val newT = s.t + 1
    val newTsSelected = Vector.fill(newThetasSelected.size)(newT)
    State(
      t = newT,
      thetas = thetasSelected ++ newThetasSelected,
      rhos = rhosSelected ++ newRhosSelected,
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
    newRhos: RealVector): (Option[(RealMatrix, RealVector, Array[Double], Vector[Int])], Option[(RealMatrix, RealVector)], Option[Double]) = {
    val dim = thetas.getColumnDimension()
    val select_ =
      (rhos.toArray().zipWithIndex.map { case (r, i) => (r, 1, i) } ++
        newRhos.toArray().zipWithIndex.map { case (r, i) => (r, 2, i) })
        .sortBy { _._1 }
        .take(keep)
        .partition { _._2 == 1 }
    val (rhosSelected, _, select) = select_._1.unzip3
    val (newRhosSelected, _, newSelect) = select_._2.unzip3
    val resSelected =
      if (select.isEmpty) None
      else {
        val thetasSelected = thetas.getSubMatrix(select, (0 until dim).toArray)
        val weightsSelected = select.map { weights(_) }.toArray
        val tsSelected = select.map { ts(_) }.toVector
        Some(
          thetasSelected,
          MatrixUtils.createRealVector(rhosSelected),
          weightsSelected,
          tsSelected)
      }
    val resNewSelected =
      if (newSelect.isEmpty) None
      else {
        val newThetasSelected =
          newThetas.getSubMatrix(newSelect, (0 until dim).toArray)
        Some(
          newThetasSelected,
          MatrixUtils.createRealVector(newRhosSelected))
      }

    val newEpsilon =
      if (rhosSelected.isEmpty && newRhosSelected.isEmpty) None
      else if (rhosSelected.isEmpty) Some(newRhosSelected.last)
      else if (newRhosSelected.isEmpty) Some(rhosSelected.last)
      else Some(max(rhosSelected.last, newRhosSelected.last))

    (resSelected, resNewSelected, newEpsilon)
  }

  def compWeights(nAlpha: Int, priorDensity: Array[Double] => Double, s: State, sigmaSquared: RealMatrix, thetasSelected: Array[Array[Double]]): Array[Double] = {
    val weightsSum = s.weights.sum
    val sThetasM = MatrixUtils.createRealMatrix(s.thetas)

    val sqrtDet2PiSigmaSquared = sqrt(abs(new LUDecomposition(
      sigmaSquared.scalarMultiply(2.0 * Pi)).getDeterminant()))
    val inverseSigmaSquared = new LUDecomposition(sigmaSquared).getSolver()
      .getInverse()
    val weightsSelected =
      (0 until thetasSelected.size).map { i =>
        val thetaI = MatrixUtils.createRealVector(thetasSelected(i))
        priorDensity(thetaI.toArray) /
          (0 until nAlpha).map { j =>
            val weightJ = s.weights(j)
            val thetaJ = sThetasM.getRowVector(j)
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


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

import mgo.tools.execution._
import mgo.tools.LinearAlgebra._
import org.apache.commons.math3.linear.MatrixUtils
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{ Try, Success, Failure }

/**
 * Parallel ABC algorithm based on APMC by Lenormand, Jabot and Deffuant (2012). MonAPMC stands for "Monoid APMC".
 *
 *  Given a stochastic function f: Vector[Double] => Vector[Double], and a value y: Vector[Double], the algorithm aims at finding what input vectors xs are likely to have resulted in the vector y through f. The objective is to estimate the probability distribution P(X|Y = y), where X represent f's input values, and Y its output values. The algorithm returns a sample of vectors that are distributed according to this distribution.
 *
 *  The simplest way to run the algorithm is to use the [[MonAPMC.run]] and [[MonAPMC.scan]] functions. The first returns the final algorithm state, and the second the sequence of states corresponding to the successive steps of the alorithm. The posterior sample is in the final state s: s.thetas contains the particles, and s.weights their weights. For example, the expected value of the particle according to the posterior is the weighted average of the particles.
 *
 *  **Controlling the function f evaluation:** During its course, the algorithm evaluates the user-provided function f many times. It can be useful to gain control of the function evaluation. For this purpose, use the [[ExposedEval]] object returned by [[MonAPMC.exposedStep]]. Its members, [[ExposedEval.pre]] and [[ExposedEval.post]] decompose the computation around the function evaluation. [[ExposedEval.pre]] returns an intermediate state and a [[org.apache.commons.math3.linear.RealMatrix]]: (pass, xs). The rows of the matrix xs are the input vectors with which to evaluate the function f. It is up to you to use these values to construct a new matrix, ys, such that its rows are the output values of f with the corresponding row in xs. The row order must be kept. Then, give (pass, ys) back to [[ExposedEval.post]] to get the new algorithm state, and reiterate. For example, to run the algorithm sequentially until it stops:
 *
 *  {{{
 *  def f(x: Array[Double]): Array[Double] = ...
 *
 *  val step = MonAPMC.exposedStep(p)
 *
 *  var state = MonAPMC.Empty()
 *  while (!MonAPMC.stop(p, state) {
 *    (pass, xs) = step.pre(state)
 *    ys = f(xs)
 *    state = step.post(pass, ys)
 *  }
 *  }}}
 *
 *  To run the algorithm in parallel, use the functions [[MonAPMC.split]] to split a current algorithm state into 2, such that the algorithm can be continued in parallel. You can step over the two in parallel for as many steps as you like, and merge them back together with [[MonAPMC.append]].
 *
 *  {{{
 *  val s0 = MonAPMC.Empty()
 *  val (s0a, s0b) = MonAPMC.split(s0)
 *  val s1a = MonAPMC.step(s0a)
 *  val s2b = MonAPMC.step(MonAPMC.step(s0b))
 *  val s3 = MonAPMC.append(s1a, s2b)
 *  }}}
 *
 * You can split states recursively as many times as you like to run more than 2 parallel threads:
 *
 *  {{{
 *  var (s1,s2) = MonAPMC.split(MonAPMC.Empty())
 *  var (s3,s4) = MonAPMC.split(s2)
 *  /* step over all 4 states */
 *  s = MonAPMC.append(s1,MonAPMC.append(s2,MonAPMC.append(s3, s4))),
 *  }}}
 */

object MonAPMC {

  /** The type over which we are constructing a monoid. */
  sealed trait MonState
  /** The monoid identity element */
  case class Empty() extends MonState
  /** Other monoid elements*/
  case class State(t0: Int, s: APMC.State) extends MonState

  /** Monoid binary operation for MonState. */
  def append(n: Int, nAlpha: Int, s1: MonState, s2: MonState): MonState =
    (s1, s2) match {
      case (a, Empty()) => a
      case (Empty(), a) => a
      case (State(t0a, sa), State(t0b, sb)) => stepMerge(n, nAlpha, State(t0a, sa), State(t0b, sb))
    }

  def split(s: MonState): (MonState, MonState) =
    s match {
      case Empty() => (Empty(), Empty())
      case State(_, s_) => (s, State(t0 = s_.t, s = s_))
    }

  /// /!\ FIXME Take care of random generator parallelization, there is a racing issue here
  def monoidParallel(
    p: APMC.Params,
    f: (Vector[Double], util.Random) => Vector[Double],
    stepSize: Int,
    parallel: Int)(
    implicit
    rng: util.Random, ec: ExecutionContext): MonoidParallel[MonState] =
    MonoidParallel(
      empty = Empty(),
      append = (s1, s2) => {
        val s3 = append(p.n, p.nAlpha, s1, s2)
        (s1, s2, s3) match {
          case (State(_, s1_), State(_, s2_), State(_, s3_)) =>
          case _ =>
        }
        s3
      },
      split = { s =>
        val (s1, s2) = split(s)
        (s1, s2)
      },
      step = s => {
        val s1 = step(p, f, s)
        s1
      },
      parallel = parallel,
      stepSize = stepSize,
      stop = stop(p, _))

  def run(p: APMC.Params, f: (Vector[Double], util.Random) => Vector[Double],
    stepSize: Int, parallel: Int)(
    implicit
    rng: util.Random, ec: ExecutionContext): Try[MonState] = {
    monoidParallel(p, f, stepSize, parallel).run
  }

  def scan(p: APMC.Params, f: (Vector[Double], util.Random) => Vector[Double], stepSize: Int, parallel: Int)(
    implicit
    rng: util.Random, ec: ExecutionContext): Vector[MonState] =
    monoidParallel(p, f, stepSize, parallel).scan

  /** The algorithm iteration step */
  def step(p: APMC.Params, f: (Vector[Double], util.Random) => Vector[Double], s: MonState)(implicit rng: util.Random): MonState =
    exposedStep(p).run { functorVectorVectorDoubleToMatrix { _.map(f(_, rng)) } }(s)

  type StepState = Either[Matrix, (APMC.State, Int, Matrix, Matrix)]

  def preStep(n: Int, nAlpha: Int, priorSample: util.Random => Array[Double], state: MonState)(implicit rng: util.Random): (StepState, Matrix) =
    state match {
      case Empty() =>
        val thetas = APMC.initPreEval(n, priorSample)
        (Left(thetas), thetas)
      case State(t0, s) =>
        val (sigmaSquared, thetas) = APMC.stepPreEval(n, nAlpha, s)
        (Right(s, t0, sigmaSquared, thetas), thetas)
    }

  def postStep(n: Int, nAlpha: Int, priorDensity: Array[Double] => Double, observed: Array[Double], stepState: StepState, xs: Matrix)(implicit rng: util.Random) =
    stepState match {
      case Left(thetas) => State(0, APMC.initPostEval(n, nAlpha, observed, thetas, xs))
      case Right((s, t0, sigmaSquared, thetas)) =>
        State(t0, APMC.stepPostEval(n, nAlpha, priorDensity, observed, s, sigmaSquared, thetas, xs))
    }

  def exposedStep(p: APMC.Params)(implicit rng: util.Random): ExposedEval[MonState, Matrix, Either[Matrix, (APMC.State, Int, Matrix, Matrix)], Matrix, MonState] =
    ExposedEval(
      pre = preStep(p.n, p.nAlpha, p.priorSample, _),
      post = postStep(p.n, p.nAlpha, p.priorDensity, p.observed, _, _))


  def steps(s: MonState) = s match {
    case Empty() => 0
    case s: State => s.s.t
  }

  def stop(p: APMC.Params, s: MonState): Boolean = s match {
    case Empty() => return false
    case State(t0, s) => APMC.stop(p, s)
  }

  def stepMerge(n: Int, nAlpha: Int, _s1: State, _s2: State): State = {

    val (s1, s2) = if (_s1.t0 <= _s2.t0) (_s1, _s2) else (_s2, _s1)
    val thetaM1 = MatrixUtils.createRealMatrix(s1.s.thetas)
    val thetaM2 = MatrixUtils.createRealMatrix(s2.s.thetas)

    val select2NoDup =
      s2.s.ts.zipWithIndex.filter { _._1 > s2.t0 }.map { _._2 }
    val indices = (0 until thetaM1.getRowDimension()).map { (1, _) } ++
      select2NoDup.map { (2, _) }.toVector
    val selectBoth =
      indices.sortBy {
        case (b, i) =>
          if (b == 1) { s1.s.rhos(i) }
          else { s2.s.rhos(i) }
      }.take(nAlpha)
    val select1 = selectBoth.filter { _._1 == 1 }.map { _._2 }.toArray
    val select2 = selectBoth.filter { _._1 == 2 }.map { _._2 }.toArray
    val rhosSelected = select1.map { s1.s.rhos(_) } ++
      select2.map { s2.s.rhos(_) }
    val tsSelected = select1.map { s1.s.ts(_) } ++
      select2.map { s2.s.ts(_) - s2.t0 + s1.s.t }
    val newEpsilon = selectBoth.last match {
      case (b, i) => if (b == 1) { s1.s.rhos(i) } else { s2.s.rhos(i) }
    }

    val thetasSelected = select1.map { thetaM1.getRow(_) } ++ select2.map { thetaM2.getRow(_) }

    val weightsSelected =
      select1.map { s1.s.weights(_) } ++ select2.map { s2.s.weights(_) }

    State(
      t0 = s1.t0,
      s = APMC.State(
        t = s1.s.t + s2.s.t - s2.t0,
        ts = tsSelected.toVector,
        thetas = thetasSelected,
        weights = weightsSelected,
        rhos = rhosSelected,
        pAcc = select2.size.toDouble / (n - nAlpha),
        epsilon = newEpsilon))
  }
}

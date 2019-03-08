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
import org.apache.commons.math3.linear.RealMatrix
import org.apache.commons.math3.random.RandomGenerator
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.{ Try, Success, Failure }

object MonAPMC {

  /** The type over which we are constructing a monoid. */
  sealed trait MonState
  /** The monoid identity element */
  case class Empty() extends MonState
  /** Other monoid elements*/
  case class State(p: APMC.Params, s: APMC.State) extends MonState

  /** Monoid binary operation for MonState. */
  def append(s1: MonState, s2: MonState): MonState =
    (s1, s2) match {
      case (a, Empty()) => a
      case (Empty(), a) => a
      case (State(p, s1), State(_, s2)) =>
        val merged = APMC.stepMerge(p, s1, s2)
        State(p, APMC.stepMerge(p, s1, s2))
    }

  /** The initial step of the algorithm */
  def init(p: APMC.Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): MonState = State(p, APMC.init(p, f))

  /** The algorithm iteration step */
  def step(p: APMC.Params, f: Vector[Double] => Vector[Double], s: MonState)(implicit rng: RandomGenerator): MonState = s match {
    case Empty() => Empty()
    case State(p, s) => {
      val newS = APMC.step(p, f, s)
      State(p, newS)
    }
  }

  def stop(p: APMC.Params, s: MonState): Boolean = s match {
    case Empty() => return true
    case State(p, s) => APMC.stop(p, s)
  }

  def run(
    stepSize: Int, parallel: Int, p: APMC.Params,
    f: Vector[Double] => Vector[Double])(
    implicit
    rng: RandomGenerator, ec: ExecutionContext): Try[MonState] = {
    val ee = exposedEval(p)
    monoidParallel().run(
      init = Vector.fill(parallel) { () => ee.init(f) },
      step = ee.step(f),
      stepSize = stepSize,
      stop = stop(p, _))
  }
  /*monoidParallel().run(
      init = Vector.fill(parallel) { () => init(p, f) },
      step = step(p, f, _: MonState),
      stepSize = stepSize,
      stop = stop(p, _: MonState))
      */

  def scan(stepSize: Int, parallel: Int, p: APMC.Params, f: Vector[Double] => Vector[Double])(
    implicit
    rng: RandomGenerator, ec: ExecutionContext): Try[Vector[MonState]] =
    monoidParallel().scan(
      init = Vector.fill(parallel) { () => init(p, f) },
      step = step(p, f, _),
      stepSize = stepSize,
      stop = stop(p, _))

  def monoidParallel(): MonoidParallel[MonState] =
    MonoidParallel(Empty(), append _, s => (s, s))

  def exposedEval(p: APMC.Params)(implicit rng: RandomGenerator): ExposedEval[MonState, RealMatrix, Option[(APMC.State, APMC.State, RealMatrix, RealMatrix)], Vector[Double], Vector[Double]] = {
    val apmc = APMC.exposedEval(p)
    ExposedEval(
      initPreEval = apmc.initPreEval,
      initPostEval = { (thetas, xsV) =>
        val s = apmc.initPostEval(thetas, xsV)
        State(p, s)
      },
      stepPreEval = _ match {
        case Empty() => (None, Vector.empty)
        case State(p, s) =>
          val (sinit, newThetasV) = apmc.stepPreEval(s)
          (Some(sinit), newThetasV)
      },
      stepPostEval = { (sstep, newXsV) =>
        sstep match {
          case None => Empty()
          case Some(sstep_) =>
            val s = apmc.stepPostEval(sstep_, newXsV)
            State(p, s)
        }
      },
      stop = stop(p, _))
  }
}

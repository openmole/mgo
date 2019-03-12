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
import mgo.tools.LinearAlgebra.functorVectorVectorDoubleToRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
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
        State(p, stepMerge(p, s1, s2))
    }

  def split(s: MonState): (MonState, MonState) =
    s match {
      case Empty() => (Empty(), Empty())
      case State(p, s_) => (s, State(p, s_.copy(t0 = s_.t)))
    }

  def monoidParallel(
    p: APMC.Params,
    f: Vector[Double] => Vector[Double],
    stepSize: Int,
    parallel: Int)(
    implicit
    rng: RandomGenerator, ec: ExecutionContext): MonoidParallel[MonState] =
    MonoidParallel(
      empty = Empty(),
      append = (s1, s2) => {
        val s3 = append(s1, s2)
        (s1, s2, s3) match {
          case (State(_, s1_), State(_, s2_), State(_, s3_)) =>
          // println((s3_.t0, s3_.t).toString ++ " <- " ++
          //   (s1_.t0, s1_.t).toString ++ " + " ++
          //   (s2_.t0, s2_.t).toString)
          case _ =>
        }
        s3
      },
      split = { s =>
        val (s1, s2) = split(s)
        //(s, s1, s2) match {
        //  case (State(_, s_), State(_, s1_), State(_, s2_)) =>
        //    println((s_.t0, s_.t).toString ++ " -> " ++ (s1_.t0, s1_.t).toString ++ ", " ++ (s2_.t0, s2_.t).toString)
        //}
        (s1, s2)
      },
      init = Vector.fill(parallel) { () => init(p, f) },
      step = s => {
        val s1 = step(p, f, s)
        // (s, s1) match {
        // case (State(_, s_), State(_, s1_)) =>
        // println("step " ++ (s_.t0, s_.t).toString ++ " > " ++ (s1_.t0, s1_.t).toString)
        // }
        s1
      },
      stepSize = stepSize,
      stop = stop(p, _))

  def run(p: APMC.Params, f: Vector[Double] => Vector[Double],
    stepSize: Int, parallel: Int)(
    implicit
    rng: RandomGenerator, ec: ExecutionContext): Try[MonState] = {
    monoidParallel(p, f, stepSize, parallel).run
  }

  def scan(p: APMC.Params, f: Vector[Double] => Vector[Double],
    stepSize: Int, parallel: Int)(
    implicit
    rng: RandomGenerator, ec: ExecutionContext): Vector[MonState] =
    monoidParallel(p, f, stepSize, parallel).scan

  /** The initial step of the algorithm */
  def init(p: APMC.Params, f: Vector[Double] => Vector[Double])(implicit rng: RandomGenerator): MonState = {
    exposedInit(p).run(functorVectorVectorDoubleToRealMatrix(_.map { f }))(())
  }

  def exposedInit(p: APMC.Params)(implicit rng: RandomGenerator): ExposedEval[Unit, MonState, RealMatrix, RealMatrix, RealMatrix] = {
    val apmcInit = APMC.exposedInit(p)
    ExposedEval(
      pre = apmcInit.pre,
      post = Function.untupled(
        apmcInit.post.tupled andThen { State(p, _) }))
  }

  /** The algorithm iteration step */
  def step(p: APMC.Params, f: Vector[Double] => Vector[Double], s: MonState)(implicit rng: RandomGenerator): MonState =
    exposedStep(p).run {
      _.map {
        functorVectorVectorDoubleToRealMatrix { _.map(f) }
      }
    }(s)

  def exposedStep(p: APMC.Params)(implicit rng: RandomGenerator): ExposedEval[MonState, MonState, Option[(APMC.State, APMC.State, RealMatrix, RealMatrix)], Option[RealMatrix], Option[RealMatrix]] = {
    val apmcStep = APMC.exposedStep(p)
    ExposedEval(
      pre = _ match {
        case Empty() => (None, None)
        case State(_, s) =>
          val (a, b) = apmcStep.pre(s)
          (Some(a), Some(b))
      },
      post = { (pass: Option[(APMC.State, APMC.State, RealMatrix, RealMatrix)], xs: Option[RealMatrix]) =>
        (pass, xs) match {
          case (Some(pass_), Some(xs_)) => State(p, apmcStep.post(pass_, xs_))
          case _ => Empty()
        }
      })
  }

  def stop(p: APMC.Params, s: MonState): Boolean = s match {
    case Empty() => return true
    case State(p, s) => APMC.stop(p, s)
  }

  def stepMerge(p: APMC.Params, s1: APMC.State, s2: APMC.State): APMC.State = {
    val select2NoDup =
      s2.ts.zipWithIndex.filter { _._1 > s2.t0 }.map { _._2 }
    val indices = (0 until s1.thetas.getRowDimension()).map { (1, _) } ++
      select2NoDup.map { (2, _) }.toVector
    val selectBoth =
      indices.sortBy {
        case (b, i) =>
          if (b == 1) { s1.rhos.getEntry(i) }
          else { s2.rhos.getEntry(i) }
      }.take(p.nAlpha)
    val select1 = selectBoth.filter { _._1 == 1 }.map { _._2 }.toArray
    val select2 = selectBoth.filter { _._1 == 2 }.map { _._2 }.toArray
    val rhosSelected = select1.map { s1.rhos.getEntry(_) } ++
      select2.map { s2.rhos.getEntry(_) }
    val tsSelected = select1.map { s1.ts(_) } ++
      select2.map { s2.ts(_) - s2.t0 + s1.t }
    val newEpsilon = selectBoth.last match {
      case (b, i) => if (b == 1) { s1.rhos.getEntry(i) }
      else { s2.rhos.getEntry(i) }
    }
    val thetasSelected = MatrixUtils.createRealMatrix(
      (select1.map { s1.thetas.getRow(_) } ++
        select2.map { s2.thetas.getRow(_) }).toArray)
    val weightsSelected =
      select1.map { s1.weights(_) } ++ select2.map { s2.weights(_) }
    APMC.State(
      thetas = thetasSelected,
      t0 = s1.t0,
      t = s2.t,
      ts = tsSelected.toVector,
      weights = weightsSelected,
      rhos = MatrixUtils.createRealVector(rhosSelected),
      pAcc = s2.pAcc,
      epsilon = newEpsilon)
  }

  /*def exposedInit(p: APMC.Params)(implicit rng: RandomGenerator): ExposedEval[Unit, MonState, RealMatrix, Vector[Vector[Double]], Vector[Vector[Double]]] =
    ExposedEval(
      pre = { Unit =>
        val thetasRM = APMC.initPreEval(p)
        val thetasV = thetasRM.getData().toVector.map { _.toVector }
        (thetasRM, thetasV)
      },
      post = { (thetas, xsV) =>
        val xs = MatrixUtils.createRealMatrix(xsV.toArray.map { _.toArray })
        State(p, APMC.initPostEval(p, thetas, xs))
      })
      */

  /*def exposedStep(p: APMC.Params)(implicit rng: RandomGenerator): ExposedEval[MonState, MonState, Option[(APMC.State, APMC.State, RealMatrix, RealMatrix)], Vector[Vector[Double]], Vector[Vector[Double]]] =
    ExposedEval(
      pre = _ match {
        case Empty() => (None, Vector.empty)
        case State(_, s) =>
          val (sPreEval, sigmaSquared, newThetas) = APMC.stepGenPreEval(p, s)
          val newThetasV = newThetas.getData().toVector.map { _.toVector }
          (Some(s, sPreEval, sigmaSquared, newThetas), newThetasV)
      },
      post = (sstep, newXsV) =>
        sstep match {
          case None => Empty()
          case Some((s, sPreEval, sigmaSquared, newThetas)) =>
            val newXs = MatrixUtils.createRealMatrix(
              newXsV.toArray.map { _.toArray })
            val newS = APMC.stepGenPostEval(p, sPreEval, sigmaSquared, newThetas, newXs)
            State(p, newS)
        })
        */

}

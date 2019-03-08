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

package mgo.tools.execution

import org.apache.commons.math3.random.RandomGenerator
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }

import mgo.abc._

/**
 * @param empty The monoid identity element, such that: append(empty, s) == s, append(s, empty) == s
 * @param append The monoid binary operator to combine 2 states
 * @param split Split the current state. The second member of the tuple is to be sent as input to the next step. The first is to become the new current state. This function can compute new states or simply duplicate its input, untouched, e.g. split(s) == (s, s).
 */
case class MonoidParallel[S](
  empty: S,
  append: (S, S) => S,
  split: S => (S, S)) {

  def run(
    init: Vector[() => S],
    step: S => S,
    stepSize: Int,
    stop: S => Boolean,
    )(
    implicit
    rng: RandomGenerator,
    ec: ExecutionContext): Try[S] = {

    @tailrec
    def go(curS: S, running: Vector[Future[S]]): Try[S] = {
      MonoidParallel.waitForNext(running) match {
        case Success((res, left)) => {
          val (newS, inputS) = split(append(curS, res))
          if (stop(newS)) { new Success(newS) }
          else go(newS, left :+ MonoidParallel.fullStepASync(step, stepSize)(inputS))
        }
        case Failure(e) => Failure(e)
      }
    }

    go(empty, init.map { i => Future(i()) })
  }

  def scan(
    init: Vector[() => S],
    step: S => S,
    stepSize: Int,
    stop: S => Boolean)(
    implicit
    rng: RandomGenerator,
    ec: ExecutionContext): Try[Vector[S]] = {

    @tailrec
    def go(curS: S, running: Vector[Future[S]], acc: Try[Vector[S]]): Try[Vector[S]] = {
      acc match {
        case Failure(err) =>
          new Failure(new Throwable("Failure in MonoidParallel.scan accumulator", err))
        case Success(acc_) =>
          MonoidParallel.waitForNext(running) match {
            case Success((res, remaining)) => {
              val (newS, inputS) = split(append(curS, res))
              if (stop(newS)) new Success( acc_ :+ newS )
              else go(newS, remaining :+ MonoidParallel.fullStepASync(step, stepSize)(inputS), new Success(acc_ :+ newS))
            }
            case Failure(err) =>
              new Failure(
                new Throwable("Failure when waiting for next in MonoidParallel.scan", err))
          }
      }
    }

    go(empty, init.map { i => Future(i()) }, new Success(Vector.empty))
  }

}

object MonoidParallel {
  @tailrec
  def fullStep[S](step: S => S, stepSize: Int)(s: S): S =
    if (stepSize == 0) s
    else fullStep(step, stepSize - 1)(step(s))

  def fullStepASync[S](step: S => S, stepSize: Int)(s: S)(
    implicit
    ec: ExecutionContext): Future[S] = Future(fullStep(step, stepSize)(s))

  @tailrec
  def waitForNext[S](running: Vector[Future[S]]): Try[(S, Vector[Future[S]])] = {
    val r = running.head
    val rs = running.tail
    r.value match {
      case None => waitForNext(rs :+ r)
      case Some(Failure(error)) => new Failure(new Throwable("Error in a running job.", error))
      case Some(Success(a)) => new Success((a, rs))
    }
  }

}

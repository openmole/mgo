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
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }

object MonoidParallelism {

  def run[S](append: (S, S) => S, mempty: S)(
    init: Vector[() => S],
    step: S => S,
    stop: S => Boolean,
    stepSize: Int)(
    implicit
    rng: RandomGenerator,
    ec: ExecutionContext): Try[S] = {

    def go(curS: S, running: Vector[Future[S]]): Try[S] = {
      waitForNext(running) match {
        case Success((res, left)) => {
          val newS = append(curS, res)
          if (stop(newS)) { new Success(newS) }
          else go(newS, left :+ theStep(newS))
        }
        case Failure(e) => Failure(e)
      }
    }

    def theStep(s: S): Future[S] = stepASync(fullStep(step, stepSize) _)(s)

    go(mempty, init.map { i => Future(i()) })
  }

  def scan[S](append: (S, S) => S, mempty: S)(
    init: Vector[() => S],
    step: S => S,
    stop: S => Boolean,
    stepSize: Int)(
    implicit
    rng: RandomGenerator,
    ec: ExecutionContext): Try[Vector[S]] = {

    def go(curS: S, running: Vector[Future[S]]): Try[Vector[S]] = {
      waitForNext(running) match {
        case Success((res, left)) => {
          val newS = append(curS, res)
          if (stop(newS)) { new Success(Vector(newS)) }
          else {
            go(newS, left :+ theStep(newS)) match {
              case Success(next) => new Success(newS +: next)
              case f => f
            }
          }
        }
        case Failure(e) => Failure(e)
      }

    }

    def theStep(s: S): Future[S] = stepASync(fullStep(step, stepSize) _)(s)

    go(mempty, init.map { i => Future(i()) })
  }

  def fullStep[S](step: S => S, stepSize: Int)(s: S): S = Iterator.iterate(s)(step).drop(stepSize - 1).next()

  def stepASync[S](step: S => S)(s: S)(
    implicit
    ec: ExecutionContext): Future[S] = Future(step(s))

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

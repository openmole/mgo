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
  split: S => (S, S),
  step: S => S,
  parallel: Int,
  stepSize: Int,
  stop: S => Boolean) {

  def run(
    implicit
    ec: ExecutionContext): Try[S] = {

    @tailrec
    def go(curS: S, running: Vector[Future[S]]): Try[S] = {
      waitForNextT(running) match {
        case Success((res, left)) => {
          val newS = append(curS, res)
          if (stop(newS)) { new Success(newS) }
          else {
            val (newS_1, newS_2) = split(newS)
            go(newS_1, left :+ Future(fullStep(step, stepSize, newS_2)))
          }
        }
        case Failure(e) => Failure(e)
      }
    }

    go(empty, init(empty, parallel).map { i => Future(i) })
  }

  def scan(
    implicit
    ec: ExecutionContext): Vector[S] = {

    @tailrec
    def go(curS: S, running: Vector[Future[S]], acc: Vector[S]): Vector[S] = {
      waitForNext(running) match {
        case (res, remaining) => {
          val newS = append(curS, res)
          if (stop(newS)) { acc :+ newS }
          else {
            val (newS_1, newS_2) = split(newS)
            go(newS_1, remaining :+ Future(fullStep(step, stepSize, newS_2)), acc :+ newS_1)
          }
        }
      }
    }

    go(empty, init(empty, parallel).map { i => Future(i) }, Vector.empty)
  }

  def init(start: S, n: Int): Vector[S] = {
    if (n <= 0) Vector.empty
    else if (n == 1) Vector(start)
    else {
      val (s1, s2) = split(start)
      (s2 +: init(s1, n - 1))
    }
  }

  @tailrec
  final def fullStep(step: S => S, stepSize: Int, s: S): S = {
    if (stepSize <= 0) s
    else fullStep(step, stepSize - 1, step(s))
  }

  @tailrec
  final def waitForNextT(running: Vector[Future[S]]): Try[(S, Vector[Future[S]])] = {
    val r = running.head
    val rs = running.tail
    r.value match {
      case None => waitForNextT(rs :+ r)
      case Some(Failure(error)) =>
        new Failure(new Throwable("Error in a running job: " ++ error.toString))
      case Some(Success(a)) => new Success((a, rs))
    }
  }

  @tailrec
  final def waitForNext(running: Vector[Future[S]]): (S, Vector[Future[S]]) = {
    val r = running.head
    val rs = running.tail
    r.value match {
      case None => waitForNext(rs :+ r)
      case Some(ta) => (ta.get, rs)
    }
  }
}

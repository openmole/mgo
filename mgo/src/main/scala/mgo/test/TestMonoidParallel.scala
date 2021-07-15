/*
 * Copyright (C) 2019 Guillaume Chérel
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

package mgo.test

import mgo.tools.execution.MonoidParallel
import org.apache.commons.math3.random.Well1024a

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

object SumMonoidParallel extends App {

  implicit val rng: Well1024a = new Well1024a()
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  val maxSum = 10

  val empty = 0
  val append: (Int, Int) => Int = { (a: Int, b: Int) => a + b }
  val split: Int => (Int, Int) = { (a: Int) => (a, 0) }
  val step: Int => Int = (_: Int) + 1
  val stop: Int => Boolean = (_: Int) >= maxSum

  {
    val parallel = 1
    val stepSize = 1
    val res = MonoidParallel[Int](empty, append, split, step, parallel, stepSize, stop).scan
    println(res)
  }

  {
    val parallel = 1
    val stepSize = 2
    val res = MonoidParallel[Int](empty, append, split, step, parallel, stepSize, stop).scan
    println(res)
  }
}


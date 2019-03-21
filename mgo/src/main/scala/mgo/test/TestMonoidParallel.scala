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

import mgo.evolution._
import mgo.evolution.contexts._
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.distribution.MixtureMultivariateNormalDistribution
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.Well1024a
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import scala.concurrent.ExecutionContext
import scala.math._
import scala.util.{ Try, Failure, Success }

import mgo.tools.execution.MonoidParallel

object SumMonoidParallel extends App {

  implicit val rng = new Well1024a()
  implicit val ec = ExecutionContext.global

  val maxSum = 10

  val empty = 0
  val append = { (a: Int, b: Int) => a + b }
  val split = { a: Int => (a, 0) }
  val step = (_: Int) + 1
  val stop = (_: Int) >= maxSum

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


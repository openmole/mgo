/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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
package mgo

import math._
import scala.util.Random

package object test {

  def average(s: Seq[Double]) = s.sum / s.size

  object sphere {
    def scale(s: Vector[Double]): Vector[Double] = s.map(_.scale(-2, 2))
    def compute(i: Vector[Double]): Double = scale(i).map(x => x * x).sum
  }

  object noisySphere {
    def scale(s: Vector[Double]): Vector[Double] = sphere.scale(s)
    def compute(rng: Random, v: Vector[Double]) =
      sphere.compute(v) + rng.nextGaussian() * 0.5 * math.sqrt(sphere.compute(v))
  }

  object rastrigin {
    def scale(s: Vector[Double]): Vector[Double] = s.map(_.scale(-5.12, 5.12))
    def compute(i: Vector[Double]): Double = {
      val scaled = scale(i)
      10 * scaled.size + scaled.map(x => (x * x) - 10 * math.cos(2 * Pi * x)).sum
    }
  }

  def himmelblau(x: Double, y: Double) = {
    def z(x: Double, y: Double) =
      pow(pow(x, 2) + y - 11, 2) + pow(x + pow(y, 2) - 7, 2)

    z(x.scale(-4.5, 4.5), y.scale(-4.5, 4.5))
  }

  def griewangk(g: Vector[Double]) = {
    val values = g.map(_.scale(-600, 600))
    1.0 + values.map(x => math.pow(x, 2.0) / 4000).sum - values.zipWithIndex.map { case (x, i) => x / math.sqrt(i + 1.0) }.map(math.cos).reduce(_ * _)
  }

  def rosenbrock(x: Double, y: Double) = {
    val sx = x.scale(-2048.0, 2048.0)
    val sy = y.scale(-2048.0, 2048.0)
    pow(1 - sx, 2) + 100 * pow(sy - pow(sx, 2), 2)
  }

  // Simple MG Function created by Schaffer for 1985 VEGA paper
  def schaffer(x: Double) = {
    val sx = x.scale(-100000.0, 100000.0)
    Seq(pow(sx, 2), pow(x - 2, 2))
  }

  object zdt4 {

    def scale(s: Vector[Double]): Vector[Double] = s.map(_.scale(0.0, 5.0))

    def compute(genome: Vector[Double]): Vector[Double] = {
      val genomeSize = genome.size

      def g(x: Seq[Double]) = 1 + 10 * (genomeSize - 1) + x.map { i => pow(i, 2) - 10 * cos(4 * Pi * i) }.sum

      def f(x: Seq[Double]) = {
        val gx = g(x)
        gx * (1 - sqrt(genome(0) / gx))
      }

      val scaled = scale(genome)
      Vector(scaled(0), f(scaled.tail))
    }

  }

}
/*
 * Copyright (C) Guillaume Chérel 06/05/14
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

package mgo.test

import mgo.evolution.*
import niche.*
import org.apache.commons.math3.distribution.MultivariateNormalDistribution



object PatternSquare:
  case class Square(center: Vector[Double], size: Double, grid: Int):
    def dimension = center.size

  def inSquare(square: Square, point: Vector[Double]) =
    (point zip square.center).forall: (c, sc) =>
      val lowBound = sc - square.size / 2
      val highBound = sc + square.size / 2
      c >= lowBound && c < highBound

  def patternIntern(square: Square, point: Vector[Double]) =
    def grid(size: Int, x: Vector[Double]) = x.map(_ * size).map(_.floor.toInt)

    val z = 1 / square.size
    grid(square.grid, (point zip square.center).map((c, sc) => (c - sc + square.size / 2) * z))

  def pattern(ps: PatternSquare, point: Vector[Double]) =
    ps.squares.zipWithIndex.find((s, _) => inSquare(s, point)) match
      case None => Vector.fill(point.size + 1)(-1)
      case Some(s, i) => Vector(i) ++ patternIntern(s, point)

  def patternDensity(ps: PatternSquare, p: Vector[Int]) =
    if isFallbackPattern(p)
    then patternDensityForRemaining(ps)
    else patternDensityForPatternInSquare(ps.squares(p.head))

  def volume(square: Square) = math.pow(square.size, square.dimension)

  def patternDensityForPatternInSquare(square: Square) = volume(square) / math.pow(square.grid, square.dimension)

  def patternDensityForRemaining(patternSquare: PatternSquare) = 1.0 - patternSquare.squares.map(volume).sum

  def isFallbackPattern(p: Vector[Int]) = p.head == -1

  def allPatterns2D(patternSquare: PatternSquare): Vector[Vector[Int]] =
    Vector(Vector(-1, -1, -1)) ++ {
      for
        (s, i) <- patternSquare.squares.zipWithIndex
        x <- 0 until s.grid
        y <- 0 until s.grid
      yield Vector(i, x, y)
    }

  val benchmarkPattern = PatternSquare(
    PatternSquare.Square(Vector(0.5, 0.5), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.25, 0.75), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.25), 0.01, 10),
    PatternSquare.Square(Vector(0.75, 0.75), 0.01, 10)
  )


  val oneSquare = PatternSquare(
    PatternSquare.Square(Vector(0.5, 0.5), 1.0, 20)
  )

case class PatternSquare(squares: PatternSquare.Square*)



object SquarePPSE extends App:

  import algorithm.*
  import algorithm.PPSE.*

  val inputsDistribution = new MultivariateNormalDistribution(Array(0.50, 0.50), Array(Array(0.001, 0.0), Array(0.0, 0.001)))

  val ppse = PPSE(
    lambda = 10,
    phenotype =
      (rng, c) =>
        val noise = IArray.fill(2)(rng.nextGaussian() * 0.1)
        val v = (c zip noise).map(_ + _)
        PatternSquare.pattern(PatternSquare.oneSquare, v),
    pattern = identity,
    continuous = Vector.fill(2)(C(0.3, 0.7)),
    density = Some(x => inputsDistribution.density(x.toArray))
  )

  def evolution =
    ppse.
      until(afterGeneration(500)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))
  val r = result(ppse, finalPopulation, finalState)

  println(r.sortBy(_.density).mkString("\n"))

  import better.files.*
  File("/tmp/onesquare.csv").writeText:
    r.map(l => s"${l.continuous.mkString(",")},${l.density}").mkString("\n")

/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.tools

import scala.annotation.tailrec
import scala.math.{ max, min }
import math._

/**
 * Mathematical tools for the library
 */
object Math {

  type Point2D = (Double, Double)

  implicit class Point2DDecorator(p: Point2D) {
    def x = p._1
    def y = p._2
  }

  def clamp(value: Double, min_v: Double = 0.0, max_v: Double = 1.0): Double =
    max(min(value, max_v), min_v)

  /// Definintion of epsilon
  val epsilon = 1.0e-30

  def same(i1: Iterable[Double], i2: Iterable[Double]): Boolean =
    (i1.headOption, i2.headOption) match {
      case (None, None) => true
      case (None, _) => false
      case (_, None) => false
      case (Some(h1), Some(h2)) => if (abs(h2 - h1) < epsilon) same(i1.tail, i2.tail) else false
    }

  def allTheSame(i1: Seq[Iterable[Double]], i2: Seq[Iterable[Double]]) = allTheSameSorted(i1.sorted, i2.sorted)

  def allTheSameSorted(i1: Seq[Iterable[Double]], i2: Seq[Iterable[Double]]): Boolean = {
    if (i1.isEmpty || i2.isEmpty) false
    else if (i1.size == 1) allEquals(i1.head, i2)
    else if (i2.size == 1) allEquals(i2.head, i1)
    else if (same(i1.head, i2.head)) allTheSameSorted(i1.tail, i2.tail) else false
  }

  def allEquals(i: Iterable[Double], in: Seq[Iterable[Double]]) = !in.exists(i2 => !same(i, i2))

  def centroid(e: Seq[Seq[Double]]) = e.reduce((x, y) => add(x, y)).map { x => x / e.size }

  def add(x: Seq[Double], y: Seq[Double]) = x zip y map { case (x, y) => x + y }

  def squareDist(x: Seq[Double], y: Seq[Double]) = x zip y map { case (x, y) => pow(x + y, 2) } sum

  def integral(points: Seq[Point2D]) =
    if (points.size < 2) 0.0
    else
      points.sortBy(_._1).sliding(2, 1).map {
        bounds =>
          val min = bounds(0)
          val max = bounds(1)
          ((max.y + min.y) / 2) * (max.x - min.x)
      }.sum

  def surface(a: Double, b: Double, c: Double): Double = {
    val s = (a + b + c) / 2
    math.sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def surface(p1: Point2D, p2: Point2D, p3: Point2D): Double = {
    val a = euclideanNorm(p1, p2)
    val b = euclideanNorm(p2, p3)
    val c = euclideanNorm(p3, p1)
    surface(a, b, c)
  }

  def euclideanNorm(p1: Point2D, p2: Point2D) =
    math.sqrt(math.pow(p2.x - p1.x, 2) + math.pow(p2.y - p1.y, 2))

  def isUpper(line1: Point2D, line2: Point2D, c: Point2D) =
    (line2.x - line1.x) * (c.y - line1.y) - (line2.y - line1.y) * (c.x - line1.x) > 0

  def average(sequence: Seq[Double]) = sequence.sum / sequence.size

  def mse(sequence: Seq[Double]) = {
    val avg = average(sequence)
    average(sequence.map { v ⇒ math.pow(v - avg, 2) })
  }

  def multinomialDraw[T](s: Seq[(Double, T)])(implicit rng: util.Random) = {
    assert(!s.isEmpty, "Input sequence should not be empty")
    def select(remaining: List[(Double, T)], value: Double, begin: List[(Double, T)] = List.empty): (T, List[(Double, T)]) =
      remaining match {
        case (weight, e) :: tail =>
          if (value <= weight) (e, begin.reverse ::: tail)
          else select(tail, value - weight, (weight, e) :: begin)
        case _ => sys.error(s"Bug $remaining $value $begin")
      }
    val totalWeight = s.unzip._1.sum
    select(s.toList, rng.nextDouble * totalWeight)
  }

  def findInterval(s: Vector[Double], v: Double) = {
    import scala.collection.Searching._
    search(s).search(v) match {
      case InsertionPoint(x) => x - 1
      case Found(x) => x
    }
  }

}

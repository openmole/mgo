/*
 * Copyright (C) 09/01/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mgo.tools

import cats.kernel.*
import cats.Later
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.mutable
import java.lang.Math.{abs, max, min, pow, sqrt}


//  implicit def lazyOrdering[T](implicit ord: scala.Ordering[T]): scala.Ordering[Lazy[T]] = scala.Ordering.by(_.value)
//
//  implicit def lazyOrder[T](implicit OT: Order[T]): Order[Lazy[T]] = new Order[Lazy[T]] {
//    def compare(x: Lazy[T], y: Lazy[T]) = OT.compare(x.value, y.value)
//  }

implicit class ArrayDecorator[A](array: Array[A]) {
  def get(i: Int): Option[A] =
    if (i < array.size) Some(array(i)) else None

}

implicit class IteratorDecorator[A](i: Iterator[A]) {
  def nextOption: Option[A] =
    if (i.hasNext) Some(i.next) else None
}

implicit class IterableDecorator[A](t: Iterable[A]) {
  def merge(t2: Iterable[A])(op: (A, A) => A): IndexedSeq[A] = {
    val size = max(t.size, t2.size)

    val i1 = t.iterator
    val i2 = t2.iterator

    (0 until size) map {
      i =>
        val x = i1.nextOption
        val y = i2.nextOption

        (x, y) match {
          case (Some(v1), Some(v2)) => op(v1, v2)
          case (o1, o2) => o1 orElse o2 get
        }

    }
  }
}

implicit class VectorDecorator[A](xs: Vector[A]) {
  /**
   * Shadow each element of a set
   */
  def shadows[B]: IndexedSeq[Vector[A]] =
    for (i <- xs.indices; (as, bs) = xs splitAt i) yield as ++ bs.tail
}

def rndmChoice[T](t1: T, t2: T)(implicit rng: util.Random): T = {
  if (rng.nextDouble < 0.5) t1 else t2
}

implicit class ScalaToApacheRng(rng: util.Random) extends RandomGenerator {
  override def setSeed(i: Int): Unit = rng.setSeed(i)
  override def setSeed(ints: Array[Int]): Unit = ???
  override def setSeed(l: Long): Unit = rng.setSeed(l)

  override def nextBoolean(): Boolean = rng.nextBoolean
  override def nextBytes(bytes: Array[Byte]): Unit = rng.nextBytes(bytes)
  override def nextDouble(): Double = rng.nextDouble()
  override def nextLong(): Long = rng.nextLong()
  override def nextFloat(): Float = rng.nextFloat()
  override def nextGaussian(): Double = rng.nextGaussian()
  override def nextInt(): Int = rng.nextInt()
  override def nextInt(i: Int): Int = rng.nextInt(i)
}

//implicit def lazyOrdering[T](implicit ord: Ordering[T]) = tools.Lazy.lazyOrdering(ord)

/*implicit class OptionDecorator[A](option: Option[A]) {
  def getOrElse(a: A) =
    option match {
      case Some(v) => v
      case None => a
    }
}     */

implicit class GroupByOrderedImplicitImpl[A](val t: Traversable[A]) extends AnyVal {
  def groupByOrdered[K](f: A => K): collection.Map[K, List[A]] = {
    val map = mutable.LinkedHashMap[K, List[A]]()
    for (i <- t) {
      val key = f(i)
      map(key) = i :: map.getOrElse(key, Nil)
    }
    map.mapValues(_.reverse).toMap
  }
}

def time[R](label: String, block: => R): (R, Long) = {
  val t0 = java.lang.System.nanoTime()
  val result = block // call-by-name
  val t1 = java.lang.System.nanoTime()
  (result, t1 - t0)
}

/* ------------------ Math ------------------ */

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

def allTheSame(i1: Vector[Iterable[Double]], i2: Vector[Iterable[Double]]): Boolean = allTheSameSorted(i1.sorted, i2.sorted)

def allTheSameSorted(i1: Vector[Iterable[Double]], i2: Vector[Iterable[Double]]): Boolean = {
  if (i1.isEmpty || i2.isEmpty) false
  else if (i1.size == 1) allEquals(i1.head, i2)
  else if (i2.size == 1) allEquals(i2.head, i1)
  else if (same(i1.head, i2.head)) allTheSameSorted(i1.tail, i2.tail) else false
}

def allEquals(i: Iterable[Double], in: Vector[Iterable[Double]]): Boolean = !in.exists(i2 => !same(i, i2))

def centroid(e: Vector[Vector[Double]]): Vector[Double] = e.reduce((x, y) => add(x, y)).map { x => x / e.size }

def add(x: Vector[Double], y: Vector[Double]): Vector[Double] = x zip y map { case (x, y) => x + y }

def squareDist(x: Vector[Double], y: Vector[Double]): Double = x zip y map { case (x, y) => pow(x + y, 2) } sum

def integral(points: Vector[Point2D]): Double =
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
  sqrt(s * (s - a) * (s - b) * (s - c))
}

def surface(p1: Point2D, p2: Point2D, p3: Point2D): Double = {
  val a = euclideanNorm(p1, p2)
  val b = euclideanNorm(p2, p3)
  val c = euclideanNorm(p3, p1)
  surface(a, b, c)
}

def euclideanNorm(p1: Point2D, p2: Point2D) =
  sqrt(pow(p2.x - p1.x, 2) + pow(p2.y - p1.y, 2))

def isUpper(line1: Point2D, line2: Point2D, c: Point2D) =
  (line2.x - line1.x) * (c.y - line1.y) - (line2.y - line1.y) * (c.x - line1.x) > 0

def average(sequence: Vector[Double]): Double = sequence.sum / sequence.size

def mse(sequence: Vector[Double]): Double = {
  val avg = average(sequence)
  average(sequence.map { v ⇒ pow(v - avg, 2) })
}

def multinomialDraw[T](s: Vector[(Double, T)], rng: util.Random): (T, List[(Double, T)]) = {
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

def findInterval[A: Ordering](s: Vector[A], v: A): Int = {
  import scala.collection.Searching._
  s.search(v) match {
    case InsertionPoint(x) => x - 1
    case Found(x) => x
  }
}

def randomInt(random: util.Random, discrete: mgo.evolution.D): Int =
  ((random.nextDouble() * (discrete.high - discrete.low + 1)) + discrete.low).floor.toInt

def apacheRandom(random: util.Random): RandomGenerator = new org.apache.commons.math3.random.RandomGenerator:
  override def setSeed(seed: Int): Unit = ???
  override def setSeed(seed: Array[Int]): Unit = ???
  override def setSeed(seed: Long): Unit = ???

  override def nextBytes(bytes: Array[Byte]): Unit = random.nextBytes(bytes)
  override def nextInt(): Int = random.nextInt()
  override def nextInt(n: Int): Int = random.nextInt(n)
  override def nextLong(): Long = random.nextLong()
  override def nextBoolean(): Boolean = random.nextBoolean()
  override def nextFloat(): Float = random.nextFloat()
  override def nextDouble(): Double = random.nextDouble()
  override def nextGaussian(): Double = random.nextGaussian()


def memoize[A, B](f: A => B, onId: Boolean = true): A => B =
  val memo =
    if !onId
    then scala.collection.mutable.Map[A, B]()
    else
      import scala.jdk.CollectionConverters.*
      java.util.IdentityHashMap[A, B]().asScala

  memo.synchronized:
    (a: A) => memo.getOrElseUpdate(a, f(a))



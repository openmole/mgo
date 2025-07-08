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

import org.apache.commons.math3.random.RandomGenerator

def time[R](label: String, block: => R): (R, Long) =
  val t0 = java.lang.System.nanoTime()
  val result = block // call-by-name
  val t1 = java.lang.System.nanoTime()
  (result, t1 - t0)

/// Definintion of epsilon
val epsilon = 1.0e-30

def multinomialDraw[T](s: IArray[(Double, T)], rng: util.Random): (T, List[(Double, T)]) =
  assert(!s.isEmpty, "Input sequence should not be empty")
  def select(remaining: List[(Double, T)], value: Double, begin: List[(Double, T)] = List.empty): (T, List[(Double, T)]) =
    remaining match
      case (weight, e) :: tail =>
        if (value <= weight) (e, begin.reverse ::: tail)
        else select(tail, value - weight, (weight, e) :: begin)
      case _ => sys.error(s"Bug $remaining $value $begin")

  val totalWeight = s.unzip._1.sum
  select(s.toList, rng.nextDouble * totalWeight)


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

extension [A, B](f: A => B)
  def memoized: A => B =
    val memo =
      import scala.jdk.CollectionConverters.*
      java.util.IdentityHashMap[A, B]().asScala

    (a: A) =>
      memo.synchronized:
        memo.getOrElseUpdate(a, f(a))






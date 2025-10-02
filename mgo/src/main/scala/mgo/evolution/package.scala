/*
 * Copyright (C) 2012 reuillon, Guillaume Chérel
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

package mgo.evolution

import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution.stop._
import mgo.tools.execution._
import org.apache.commons.math3.random._

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

/*------------- Running the EA ------------------*/

type Trace[S, I] = (S, Vector[I]) => Unit

case class RunAlgorithm[T, I, G, S](
  t: T,
  algo: Algorithm[T, I, G, S],
  stopCondition: Option[StopCondition[S, I]] = None,
  traceOperation: Option[Trace[S, I]] = None):

  def evolution(rng: scala.util.Random, parallel: Algorithm.ParallelContext): (S, Vector[I]) =
    val initialPop = algo.initialPopulation(t, rng, parallel)
    val initialState = algo.initialState(t, rng)
    val step = algo.step(t)

    def evolve(s: S, pop: Vector[I]): (S, Vector[I]) =
      traceOperation.foreach(_(s, pop))
      if stopCondition.getOrElse(never)(s, pop)
      then (s, pop)
      else
        val (s2, p2) = step(s, pop, rng, parallel)
        evolve(s2, p2)

    evolve(initialState, initialPop)

  def until(stopCondition: StopCondition[S, I]): RunAlgorithm[T, I, G, S] = copy(stopCondition = Some(stopCondition))

  def trace(f: (S, Vector[I]) => Unit): RunAlgorithm[T, I, G, S] = copy(traceOperation = Some(f))

  def eval(rng: scala.util.Random, parallel: Boolean = false): (S, Vector[I]) =
    val context = if parallel then Algorithm.parallel else Algorithm.Sequential
    evolution(rng, context)
  // def eval(rng: Random)(implicit monadM: cats.Monad[M]) = algo.run(evolution, algo.initialState(t, rng))


implicit def toAlgorithm[T, I, G, S](t: T)(implicit algo: Algorithm[T, I, G, S]): RunAlgorithm[T, I, G, S] = RunAlgorithm(t, algo)

/** ** Stop conditions ****/

def anyReaches[M[_]: cats.Monad, I](goalReached: I => Boolean)(population: Vector[I]): Vector[I] => M[Boolean] =
  (population: Vector[I]) => population.exists(goalReached).pure[M]

def afterGeneration[I, S](g: Long): StopCondition[EvolutionState[S], I] = stop.afterGeneration[EvolutionState[S], I](g, Focus[EvolutionState[S]](_.generation))

def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))

implicit class Double2Scalable(d: Double):
  def scale(min: Double, max: Double): Double = mgo.tools.changeScale(d, 0, 1, min, max)
  def scale(s: C): Double = scale(s.low, s.high)
  //def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)

def arrayToVectorIso[A: Manifest]: Iso[IArray[A], Vector[A]] = monocle.Iso[IArray[A], Vector[A]](_.toVector)(v => IArray.from(v))
def array2ToVectorLens[A: Manifest]: Iso[Array[Array[A]], Vector[Vector[A]]] = monocle.Iso[Array[Array[A]], Vector[Vector[A]]](_.toVector.map(_.toVector))(v => v.map(_.toArray).toArray)
def intToUnsignedIntOption: Iso[Int, Option[Int]] = monocle.Iso[Int, Option[Int]](i => if (i < 0) None else Some(i))(v => v.getOrElse(-1))
def byteToUnsignedIntOption: Iso[Byte, Option[Int]] = monocle.Iso[Byte, Option[Int]](i => if i < 0 then None else Some(i))(v => v.getOrElse(-1).toByte)

def iArrayTupleToVector(p: (IArray[Double], IArray[Int])) = (Vector.from(p._1), Vector.from(p._2))

case class C(low: Double, high: Double)

object D:
  object IntegerPrecision:
    extension (t: IntegerPrecision)
      def size =
        t match
          case Byte => 1
          case Short => 2
          case Int => 4

  enum IntegerPrecision:
    case Byte, Short, Int

  val byteRange = Byte.MaxValue.toInt - Byte.MinValue
  val shortRange = Short.MaxValue.toInt - Short.MinValue

  def precision(low: Int, high: Int) =
    val interval = Math.abs(high.toLong - low)
    if interval <= byteRange
    then IntegerPrecision.Byte
    else if interval <= shortRange
    then IntegerPrecision.Short
    else IntegerPrecision.Int

  def apply(low: Int, high: Int) =
    val l = Math.min(low, high)
    val h = Math.max(low, high)
    val p = precision(l, h)
    new D(l, h, p)

case class D(low: Int, high: Int, precision: D.IntegerPrecision)



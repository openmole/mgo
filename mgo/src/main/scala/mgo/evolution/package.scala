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

package mgo

import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution.stop._
import mgo.tools.execution._
import org.apache.commons.math3.random._

import monocle._
import monocle.syntax.all._

import scala.language.higherKinds

package object evolution {

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
        if (stopCondition.getOrElse(never)(s, pop))
        then (s, pop)
        else
          val (s2, p2) = step(s, pop, rng, parallel)
          evolve(s2, p2)

      evolve(initialState, initialPop)

    def until(stopCondition: StopCondition[S, I]): RunAlgorithm[T, I, G, S] = copy(stopCondition = Some(stopCondition))

    def trace(f: (S, Vector[I]) => Unit): RunAlgorithm[T, I, G, S] = copy(traceOperation = Some(f))

    def eval(rng: scala.util.Random, parallel: Algorithm.ParallelContext = Algorithm.Sequential): (S, Vector[I]) = evolution(rng, parallel)
    // def eval(rng: Random)(implicit monadM: cats.Monad[M]) = algo.run(evolution, algo.initialState(t, rng))


  implicit def toAlgorithm[T, I, G, S](t: T)(implicit algo: Algorithm[T, I, G, S]): RunAlgorithm[T, I, G, S] = RunAlgorithm(t, algo)

  /** ** Stop conditions ****/

  def anyReaches[M[_]: cats.Monad, I](goalReached: I => Boolean)(population: Vector[I]): Vector[I] => M[Boolean] =
    (population: Vector[I]) => population.exists(goalReached).pure[M]

  def afterGeneration[I, S](g: Long): StopCondition[EvolutionState[S], I] = stop.afterGeneration[EvolutionState[S], I](g, Focus[EvolutionState[S]](_.generation))

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))

  def changeScale(v: Double, fromMin: Double, fromMax: Double, toMin: Double, toMax: Double): Double = {
    val factor = (toMax - toMin) / (fromMax - fromMin)
    (factor * (v - fromMin) + toMin)
  }

  implicit def double2Scalable(d: Double): double2Scalable = new double2Scalable(d)
  class double2Scalable(d: Double) {
    def scale(min: Double, max: Double): Double = changeScale(d, 0, 1, min, max)
    def scale(s: C): Double = scale(s.low, s.high)
    //def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  def arrayToVectorIso[A: Manifest]: Iso[Array[A], Vector[A]] = monocle.Iso[Array[A], Vector[A]](_.toVector)(v => v.toArray)
  def array2ToVectorLens[A: Manifest]: Iso[Array[Array[A]], Vector[Vector[A]]] = monocle.Iso[Array[Array[A]], Vector[Vector[A]]](_.toVector.map(_.toVector))(v => v.map(_.toArray).toArray)
  def intToUnsignedIntOption: Iso[Int, Option[Int]] = monocle.Iso[Int, Option[Int]](i => if (i < 0) None else Some(i))(v => v.getOrElse(-1))

  case class C(low: Double, high: Double)
  case class D(low: Int, high: Int)

}

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

import cats.data._
import cats.implicits._
import mgo.evolution.algorithm._
import mgo.evolution.contexts._
import mgo.evolution.stop._
import mgo.evolution.breeding._
import mgo.evolution.elitism._
import mgo.tools.execution._
import org.apache.commons.math3.random._
import mgo.tagtools._

import scala.language.higherKinds
import scala.util.Random

package object evolution extends stop.Imports {

/**** Running the EA ****/

  type Trace[M[_], I] = Kleisli[M, Vector[I], Unit]

  def noTrace[M[_]: cats.Monad, I] = Kleisli[M, Vector[I], Unit](_ => ().pure[M])

  case class RunAlgorithm[T, M[_], I, G, S](
    t: T,
    algo: Algorithm[T, M, I, G, S],
    stopCondition: Option[StopCondition[M, I]] = None,
    traceOperation: Option[Trace[M, I]] = None) {

    def evolution(implicit monadM: cats.Monad[M]) =
      for {
        initialPop <- algo.initialPopulation(t)
        finalPop <- step.fold(initialPop)(stopCondition.getOrElse(never[M, I]))
        s <- algo.state
      } yield (s, finalPop)

    def step(implicit monadM: cats.Monad[M]) =
      for {
        _ <- traceOperation.getOrElse(noTrace[M, I])
        vi <- algo.step(t)
      } yield vi

    def until(stopCondition: StopCondition[M, I]) = copy(stopCondition = Some(stopCondition))

    def trace(f: (S, Vector[I]) => Unit)(implicit ioM: IO[M], monadM: cats.Monad[M]) = {
      val trace: Trace[M, I] =
        Kleisli { is: Vector[I] =>
          for {
            s <- algo.state
            r <- ioM(() => f(s, is))
          } yield r
        }
      copy(traceOperation = Some(trace))
    }

    // def eval(rng: Random)(implicit monadM: cats.Monad[M]) = algo.run(evolution, algo.initialState(t, rng))

  }

  implicit class RunAlgorithmDSLDecorator[T, I, G, S](f: RunAlgorithm[T, mgo.tagtools.DSL, I, G, S]) {
    def eval = f.evolution.eval
    def tryEval = f.evolution.tryEval
  }

  implicit def toAlgorithm[T, I, G, S](t: T)(implicit algo: Algorithm[T, DSL, I, G, S]) = RunAlgorithm(t, algo)

  implicit def algorithmDecorator[T, M[_], I, G, S](t: T)(implicit algo: Algorithm[T, M, I, G, S]) = RunAlgorithm(t, algo)

  /** ** Stop conditions ****/

  def anyReaches[M[_]: cats.Monad, I](goalReached: I => Boolean)(population: Vector[I]): Vector[I] => M[Boolean] =
    (population: Vector[I]) => population.exists(goalReached).pure[M]

/**** Breeding ****/

  def bindB[M[_]: cats.Monad, I, G1, G2](b1: Breeding[M, I, G1], b2: Vector[G1] => Breeding[M, I, G2]): Breeding[M, I, G2] =
    Breeding((individuals: Vector[I]) => for {
      g1s <- b1(individuals)
      g2s <- b2(g1s)(individuals)
    } yield g2s)

  def zipB[M[_]: cats.Monad, I, G1, G2](b1: Breeding[M, I, G1], b2: Breeding[M, I, G2]): Breeding[M, I, (G1, G2)] = zipWithB { (g1: G1, g2: G2) => (g1, g2) }(b1, b2)

  def zipWithB[M[_]: cats.Monad, I, G1, G2, G3](f: ((G1, G2) => G3))(b1: Breeding[M, I, G1], b2: Breeding[M, I, G2]): Breeding[M, I, G3] =
    Breeding((individuals: Vector[I]) =>
      for {
        g1s <- b1(individuals)
        g2s <- b2(individuals)
      } yield (g1s, g2s).zipped.map(f))

  def mapB[M[_]: cats.Monad, I, G](op: I => M[G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.traverse[M, G](op))

  def flatMapB[M[_]: cats.Monad, I, G](op: I => M[Vector[G]]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.flatTraverse(op))

  def byNicheB[I, N, M[_]: cats.Monad, G](niche: I => N)(breeding: Breeding[M, I, G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => {
      val indivsByNiche = individuals.groupBy(niche)
      indivsByNiche.valuesIterator.toVector.traverse(breeding.apply).map(_.flatten)
    })

  def flatMapPureB[M[_]: cats.Monad, I, G](op: I => Vector[G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.flatTraverse(op(_: I).pure[M]))

/**** Expression ****/

  //  def bindE[G, P1, P2](e1: Expression[G, P1], e2: P1 => Expression[G, P2]): Expression[G, P2] =
  //    (genome: G) => e2(e1(genome))(genome)
  //
  //  def zipE[G, P1, P2](e1: Expression[G, P1], e2: Expression[G, P2]): Expression[G, (P1, P2)] =
  //    zipWithE[G, P1, P2, (P1, P2)] { (p1: P1, p2: P2) => (p1, p2) }(e1, e2)
  //
  //  def zipWithE[G, P1, P2, P3](f: (P1, P2) => P3)(e1: Expression[G, P1], e2: Expression[G, P2]): Expression[G, P3] =
  //    (genome: G) => f(e1(genome), e2(genome))
  //
  //  def asE[G, G1, P](gtog1: G => G1, express: Expression[G1, P]): Expression[G, P] = (genome: G) => express(gtog1(genome))
  //
  //  def withGenomeE[G, P](express: Expression[G, P]): Expression[G, (P, G)] = (genome: G) => (express(genome), genome)
  //
  //  def withE[G, P](f: G => P): Expression[G, P] = f

/**** Objectives ****/
  //
  //  def bindO[M[_]: cats.Monad, I](o1: Elitism[M, I], o2: Vector[I] => Elitism[M, I]): Elitism[M, I] =
  //    Elitism((phenotypes: Vector[I]) =>
  //      for {
  //        selected1s <- o1(phenotypes)
  //        selected2s <- o2(selected1s)(phenotypes)
  //      } yield selected2s)
  //
  //  def andO[M[_]: cats.Monad, I](o1: Elitism[M, I], o2: Elitism[M, I]): Elitism[M, I] =
  //    Elitism((phenotypes: Vector[I]) =>
  //      for {
  //        selected1s <- o1(phenotypes)
  //        selected2s <- o2(phenotypes)
  //      } yield selected1s.intersect(selected2s))
  //
  //  def orO[M[_]: cats.Monad, I](o1: Elitism[M, I], o2: Elitism[M, I]): Elitism[M, I] =
  //    Elitism((phenotypes: Vector[I]) =>
  //      for {
  //        selected1s <- o1(phenotypes)
  //        selected2s <- o2(phenotypes)
  //      } yield selected1s.union(selected2s))
  //
  //  def thenO[M[_]: cats.Monad, I](o1: Elitism[M, I], o2: Elitism[M, I]): Elitism[M, I] =
  //    Elitism((phenotypes: Vector[I]) =>
  //      for {
  //        selected1s <- o1(phenotypes)
  //        selected2s <- o2(selected1s)
  //      } yield selected2s)

/**** Helper functions ****/

  //  def zipWithRandom[M[_]: Applicative, G](gs: Vector[G])(implicit MR: ParallelRandom[M]): M[Vector[(util.Random, G)]] =
  //    for { rngs <- (0 until gs.size).toVector.map(_ => MR.split).sequence } yield rngs.toVector zip gs

  /**
   * This function helps chaining Kleisli arrows in for comprehensions. For example, given two Kleisli arrows
   * k of type Kleisli[M,A,B] and l of type Kleisli[M,B,C], the following are equivalent:
   *
   * k >=> l
   *
   * for {
   *   a <- k
   *   b <- thenK(l)(a)
   * } yield b
   */
  def thenK[M[_]: cats.Monad, R, A, B](k: Kleisli[M, A, B])(a: A): Kleisli[M, R, B] =
    Kleisli[M, R, B](_ => k.run(a))

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))

  def changeScale(v: Double, fromMin: Double, fromMax: Double, toMin: Double, toMax: Double) = {
    val factor = (toMax - toMin) / (fromMax - fromMin)
    (factor * (v - fromMin) + toMin)
  }

  implicit def double2Scalable(d: Double) = new {
    def scale(min: Double, max: Double): Double = changeScale(d, 0, 1, min, max)
    def scale(s: C): Double = scale(s.low, s.high)
    //def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  def arrayToVectorLens[A: Manifest] = monocle.Lens[Array[A], Vector[A]](_.toVector)(v => _ => v.toArray)
  def array2ToVectorLens[A: Manifest] = monocle.Lens[Array[Array[A]], Vector[Vector[A]]](_.toVector.map(_.toVector))(v => _ => v.map(_.toArray).toArray)
  def intToUnsignedIntOption = monocle.Lens[Int, Option[Int]](i => if (i < 0) None else Some(i))(v => _ => v.getOrElse(-1))

  trait History[P, I] {
    val lens: monocle.Lens[I, Vector[P]]
  }

  case class HistoryOps[P, I](self: I)(implicit I: History[P, I]) {
    def get: Vector[P] = I.lens.get(self)
    def set(h: Vector[P]): I = I.lens.set(h)(self)
    def mod(f: Vector[P] => Vector[P]): I = I.lens.modify(f)(self)
  }

  object ToHistoryOps {
    implicit def toHistoryOps[P, I](v: I)(implicit I: History[P, I]): HistoryOps[P, I] = HistoryOps[P, I](v)
  }

  case class C(low: Double, high: Double)
  case class D(low: Int, high: Int)

}

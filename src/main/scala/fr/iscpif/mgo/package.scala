/*
 * Copyright (C) 2012 reuillon
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

package fr.iscpif

import org.apache.commons.math3.random._
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.Random
import scalaz._
import Scalaz._
import scala.concurrent.duration._

package object mgo {

  type Population[I] = Vector[I]

  object Population {
    def empty = Vector.empty
  }

  implicit def unwrap[@specialized A, T](a: A @@ T): A = Tag.unwrap[A, T](a)
  implicit def wrap[@specialized A, T](a: A): A @@ T = Tag.apply[A, T](a)

  def identityLens[A] = monocle.Lens[A, A](identity)(v => identity)

  implicit def unitStateConverter[X](s: X): (X, Unit) = (s, Unit)

  private def changeScale(v: Double, min: Double, max: Double, boundaryMin: Double, boundaryMax: Double) = {
    val factor = (boundaryMax - boundaryMin) / (max - min)
    (factor * (v - min) + boundaryMin)
  }

  implicit def double2Scalable(d: Double) = new {
    def scale(min: Double, max: Double) = changeScale(d, 0, 1, min, max)
    def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))
  implicit def longToRandom(seed: Long) = newRNG(seed)

  implicit class LensDecorator[A, B](lens: monocle.Lens[A, B]) {
    def toScalaz = scalaz.Lens.lensu[A, B]((a, b) => lens.set(b)(a), lens.get)
  }

  implicit def monocleToScalazLens[A, B](lens: monocle.Lens[A, B]) = lens.toScalaz

  implicit class VectorStateDecorator[S, G](gen: State[S, Vector[G]]) {

    def generateFlat(lambda: Int) = {
      def flatten0(lambda: Int)(state: S, acc: List[G] = List()): (S, List[G]) =
        if (lambda <= 0) (state, acc)
        else {
          val (newState, add) = gen.map {
            _.take(lambda)
          }.run(state)
          flatten0(lambda - 1)(newState, add.toList ::: acc)
        }

      State { state: S => flatten0(lambda)(state) }.map { _.toVector }
    }

  }

  implicit class ElementStateDecorator[S, G](gen: State[S, G]) {
    def generate(lambda: Int) = gen.map(Vector(_)).generateFlat(lambda)
  }

  def randomGenomes[G](randomGenome: State[Random, G], size: Int) =
    randomGenome.generate(size).map(_.toVector)

  def evolution(algorithm: Algorithm, lambda: Int)(
    newGenome: State[Random, algorithm.G],
    express: (algorithm.G => State[Random, algorithm.P]),
    termination: Termination[algorithm.AlgorithmState]) = new {

    import algorithm._

    def expressMonad = (g: G) => algorithm.common lifts Individual(g, express)

    def step(population: Pop): State[AlgorithmState, Pop] = {

      for {
        breed <- breeding(population, lambda)
        offspring <- breed.traverseS(expressMonad)
        population <- elitism(population, offspring)
        _ <- updateGeneration
      } yield population
    }

    def eval(random: Random): Pop = run(random)._2

    def run(random: Random) = {
      @tailrec def run0(pop: Pop, state: AlgorithmState): (AlgorithmState, Pop) = {
        val (s1, res) = step(pop).run(state)
        val (s2, cond) = termination.run(s1)
        if (cond) (s2, res)
        else run0(res, s2)
      }

      def initialGenomes = newGenome.generate(lambda)

      val allRun =
        for {
          genomes <- algorithm.random.lifts(initialGenomes)
          initialPop <- genomes.traverseS(expressMonad)
          finalPop <- State[AlgorithmState, Pop] { state: AlgorithmState => run0(initialPop, state) }
        } yield finalPop

      allRun.run(algorithmState(random))
    }
  }

  @tailrec def group[I](col: List[I], acc: List[List[I]] = List())(equality: Equal[I]): List[List[I]] =
    col match {
      case Nil => acc
      case h :: t =>
        val (begin, end) = acc.span { l => !equality.equal(h, l.head) }
        val newContent = h :: end.headOption.getOrElse(Nil)
        group(t, begin ::: newContent :: end.drop(1))(equality)
    }

  type Termination[S] = State[S, Boolean]

  sealed trait Generation
  sealed trait Start

  def afterGeneration[S](max: Long)(implicit step: monocle.Lens[S, Long @@ Generation]): Termination[S] = State { state: S => (state, step.get(state) >= max) }

  def afterTime[S](max: Duration)(implicit time: monocle.Lens[S, Long @@ Start]): Termination[S] = State {
    state: S =>
      val duration = (System.currentTimeMillis - time.get(state)).millis
      (state, duration >= max)
  }

  def never[S] = State { state: S => (state, false) }

  case class History[+C](history: List[C], age: Int @@ CloneFunctions.Age = 1)

  implicit def historyHistoryLens[C] = monocle.macros.Lenser[History[C]](_.history)
  implicit def historyAgeLens[C] = monocle.macros.Lenser[History[C]](_.age)
  implicit def historyToList[C](h: History[C]) = h.history
  implicit def stateOfCToHistory[S, C](c: State[S, C]) = c.map { c => History(List(c)) }

}

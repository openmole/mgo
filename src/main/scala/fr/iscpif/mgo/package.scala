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
import scala.util.Random
import scalaz._
import Scalaz._

package object mgo {

  case class GenomeValue[V](val value: V) extends AnyVal
  implicit def veToGenomeValue[V](v: V) = GenomeValue(v)

  case class GenomeSize(val value: Int) extends AnyVal
  case class GenomeSigma[V](val value: V) extends AnyVal

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

  implicit class ListStateDecorator[S, G](gen: State[S, Vector[G]]) {

    def generateFlat(lambda: Int) = {
      def flatten0(lambda: Int)(state: S, acc: List[G] = List()): (S, List[G]) =
        if (acc.size >= lambda) (state, acc)
        else {
          val (newState, add) = gen.map {
            _.take(lambda - acc.size)
          }.run(state)
          flatten0(lambda)(newState, add.toList ::: acc)
        }

      State { state: S => flatten0(lambda)(state) }.map { _.toVector }
    }

  }

  implicit class ElementStateDecorator[S, G](gen: State[S, G]) {
    def generate(lambda: Int) = gen.map(Vector(_)).generateFlat(lambda)
  }

  def evolution(algorithm: Algorithm)(
    randomGenome: State[Random, algorithm.G],
    express: (algorithm.G => State[Random, algorithm.P]),
    termination: State[algorithm.AlgorithmState, Boolean]) = new {

    import algorithm._

    def step(population: Pop): State[AlgorithmState, Pop] = {
      def expressMonad(g: G) = State { state: AlgorithmState => (state, Individual[G, P](g, express).eval(state.random)) }

      def randomIfEmpty =
        if (population.content.isEmpty) AlgorithmState.random.lifts(randomGenome).generate(mu).map(_.toVector)
        else breeding(population)

      for {
        breed <- randomIfEmpty
        offspring <- breed.traverseS { g => expressMonad(g) }
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

      val allRun = State[AlgorithmState, Pop] { state => run0(Population.empty, state) }

      allRun.run(algorithmState(random))
    }
  }

}

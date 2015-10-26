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
import scalaz._

package object mgo {

  case class GenomeValue[V](val value: V) extends AnyVal
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

  implicit class StateIteratorDecorator[S <: { def terminated: Boolean }](i: Iterator[S]) {
    def last = i.drop(1).dropWhile { !_.terminated }.next
    def untilConverged(f: S => Unit) = i.drop(1).dropWhile { s => f(s); !s.terminated }.next
  }

  //TODO: unused rng and newRNG functions?
  object rng {
    implicit def rng = newRNG
  }

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))
  def newRNG = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a)))

  implicit class LensDecorator[A, B](lens: monocle.Lens[A, B]) {
    def toScalaz = scalaz.Lens.lensu[A, B]((a, b) => lens.set(b)(a), lens.get)
  }

  implicit def monocleToScalazLens[A, B](lens: monocle.Lens[A, B]) = lens.toScalaz

  implicit class ElementStateDecorator[S, G](gen: State[S, G]) {
    def generate(lambda: Int) = gen.map(Vector(_)).generateFlat(lambda)

  }
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

}

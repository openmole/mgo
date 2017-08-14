/*
 * Copyright (C) 10/07/2017 Guillaume Chérel
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
package mgo.algorithm.monteCarlo

import monocle.macros.{ GenLens, Lenses }

import scala.language.higherKinds
import scala.collection.immutable._
import mgo._
import mgo.algorithm._
import mgo.ranking._
import mgo.breeding._
import mgo.elitism._
import mgo.contexts._
import tools._
import cats._
import cats.data._
import cats.implicits._
import mgo.algorithm.GenomeVectorDouble._
import freedsl.dsl
import freedsl.dsl.dsl
import freedsl.tool._
import freedsl.io._
import freedsl.random._

object MCSampling {

  def interpreter(s: EvolutionState[Unit]) =
    dsl.merge(
      Random.interpreter(s.random),
      StartTime.interpreter(s.startTime),
      Generation.interpreter(s.generation),
      IO.interpreter
    )

  val context = dsl.merge(Random, StartTime, Generation, IO)
  import context.implicits._

  implicit def mcSamplingAlgorithm[MCSampling, Sample, Evaluated](
    initialSamples: MCSampling => Vector[Evaluated],
    mcstep: MCSampling => Kleisli[context.M, Vector[Evaluated], Vector[Evaluated]]) =
    new Algorithm[MCSampling, context.M, Evaluated, Sample, EvolutionState[Unit]] {
      def initialState(t: MCSampling, rng: util.Random): EvolutionState[Unit] =
        EvolutionState(random = rng, s = ())

      def initialPopulation(t: MCSampling): context.M[Vector[Evaluated]] = initialSamples(t).pure[context.M]

      def step(t: MCSampling): Kleisli[context.M, Vector[Evaluated], Vector[Evaluated]] =
        for {
          result <- mcstep(t)
          _ <- Kleisli.lift(implicitly[Generation[context.M]].increment)
        } yield result

      def state: context.M[EvolutionState[Unit]] = mgo.algorithm.state[context.M, Unit](())

      def run[A](m: context.M[A], s: EvolutionState[Unit]): A = interpreter(s).run(m).right.get
    }
}

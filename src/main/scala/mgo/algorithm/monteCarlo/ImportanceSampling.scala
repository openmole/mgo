/*
 * Copyright (C) 11/08/2017 Guillaume Chérel
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

object ImportanceSampling {

  def interpreter(s: EvolutionState[Unit]) =
    dsl.merge(
      Random.interpreter(s.random),
      StartTime.interpreter(s.startTime),
      Generation.interpreter(s.generation),
      IO.interpreter
    )

  val context = dsl.merge(Random, StartTime, Generation, IO)
  import context.implicits._

  object ImportanceSampling {

    implicit def isAlgorithm = new Algorithm[ImportanceSampling, context.M, Evaluated, Sample, EvolutionState[Unit]] {
      def initialState(t: ImportanceSampling, rng: util.Random): EvolutionState[Unit] =
        EvolutionState(random = rng, s = ())

      def initialPopulation(t: ImportanceSampling): context.M[Vector[Evaluated]] =
        (Vector.empty[Evaluated]).pure[context.M]

      def step(t: ImportanceSampling): Kleisli[context.M, Vector[Evaluated], Vector[Evaluated]] =
        Kleisli { samples =>
          for {
            newSample <- implicitly[Random[context.M]].use(t.qSample)
            _ <- implicitly[Generation[context.M]].increment
            p = t.pPdf(newSample)
            q = t.qPdf(newSample)
          } yield samples :+ Evaluated(Sample(newSample), p, p / q)
        }

      def state: context.M[EvolutionState[Unit]] = mgo.algorithm.state[context.M, Unit](())

      def run[A](m: context.M[A], s: EvolutionState[Unit]): A = interpreter(s).run(m).right.get
    }
  }

  case class ImportanceSampling(
    qSample: util.Random => Vector[Double],
    qPdf: Vector[Double] => Double,
    pPdf: Vector[Double] => Double)

  @Lenses case class Sample(values: Vector[Double])
  @Lenses case class Evaluated(sample: Sample, probability: Double, importance: Double)

  def samples(samples: Vector[Evaluated]): Vector[Vector[Double]] =
    samples.map((Evaluated.sample composeLens Sample.values).get)
}

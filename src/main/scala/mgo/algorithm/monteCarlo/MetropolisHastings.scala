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

import scala.math

import MCSampling.context.implicits._

case class MetropolisHastings(
  initialSample: MetropolisHastings.Evaluated,
  qSample: Vector[Double] => util.Random => Vector[Double],
  qPdf: (Vector[Double], Vector[Double]) => Double,
  pPdf: Vector[Double] => Double)

object MetropolisHastings {

  implicit def isAlgorithm = MCSampling.mcSamplingAlgorithm[MetropolisHastings, Sample, Evaluated](
    initialSample, step)

  def initialSample(t: MetropolisHastings): Vector[Evaluated] = Vector(t.initialSample)

  def step(t: MetropolisHastings): Kleisli[MCSampling.context.M, Vector[Evaluated], Vector[Evaluated]] =
    Kleisli { samples =>
      for {
        uniform <- implicitly[Random[MCSampling.context.M]].nextDouble
        lastSample = samples.last
        proposal <- implicitly[Random[MCSampling.context.M]].use(t.qSample(lastSample.sample.values))
        pLast = lastSample.probability
        pNew = t.pPdf(proposal)
        qJump = t.qPdf(lastSample.sample.values, proposal)
        qReturn = t.qPdf(proposal, lastSample.sample.values)
        acceptanceProbability = math.min(1, (pNew * qReturn) / (pLast * qJump))
      } yield if (uniform < acceptanceProbability) {
        samples :+ Evaluated(Sample(proposal), pNew)
      } else {
        samples :+ lastSample
      }
    }

  @Lenses case class Sample(values: Vector[Double])
  @Lenses case class Evaluated(sample: Sample, probability: Double)

  def samples(samples: Vector[Evaluated]): Vector[Vector[Double]] =
    samples.map((Evaluated.sample composeLens Sample.values).get)
}

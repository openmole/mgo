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

import MCSampling.context.implicits._

case class ImportanceSampling(
  qSample: util.Random => Vector[Double],
  qPdf: Vector[Double] => Double,
  pPdf: Vector[Double] => Double)

object ImportanceSampling {

  implicit def isAlgorithm = MCSampling.mcSamplingAlgorithm[ImportanceSampling, Sample, Evaluated](step)

  def step(t: ImportanceSampling): Kleisli[MCSampling.context.M, Vector[Evaluated], Vector[Evaluated]] =
    Kleisli { samples =>
      for {
        newSample <- implicitly[Random[MCSampling.context.M]].use(t.qSample)
        p = t.pPdf(newSample)
        q = t.qPdf(newSample)
      } yield samples :+ Evaluated(Sample(newSample), p, p / q)
    }

  @Lenses case class Sample(values: Vector[Double])
  @Lenses case class Evaluated(sample: Sample, probability: Double, importance: Double)

  def samples(samples: Vector[Evaluated]): Vector[Vector[Double]] =
    samples.map((Evaluated.sample composeLens Sample.values).get)
}

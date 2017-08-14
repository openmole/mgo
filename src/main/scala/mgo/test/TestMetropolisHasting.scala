/*
 * Copyright (C) Guillaume Chérel 11/08/2017
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package mgo.test

import math._

import mgo._

import algorithm.monteCarlo.MetropolisHastings
import algorithm.monteCarlo.MCSampling.context.implicits._

object BimodalMetropolisHastings extends App {

  def normalSample(mean: Vector[Double])(r: util.Random) =
    Vector(r.nextGaussian * 100 + mean.head)

  def normalPdf(mean: Vector[Double], x: Vector[Double]) =
    (1.0 / (100 * sqrt(2 * Pi))) * exp(-pow(x.head - mean.head, 2) / (2.0 * pow(100, 2)))

  def bimodalPdfProportional(x: Vector[Double]) =
    0.3 * exp(-0.2 * pow(x.head, 2)) + 0.7 * exp(-0.2 * pow(x.head - 10, 2))

  val mh = MetropolisHastings(
    initialSample = MetropolisHastings.Evaluated(
      MetropolisHastings.Sample(Vector(0.0)),
      bimodalPdfProportional(Vector(0.0))),
    qSample = normalSample,
    qPdf = normalPdf,
    pPdf = bimodalPdfProportional
  )

  val (finalState, finalPopulation) =
    run(mh).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new util.Random(42))

  println(finalPopulation.mkString("\n"))

  def bin(x: Double) = x.toInt

  val bins = finalPopulation.map({ e => bin(e.sample.values.head) })

  val counts = bins.groupBy({ b => b }).mapValues(_.size).toVector.sortBy({ t => t._1 })

  counts.foreach { case (b, c) => println("bin " ++ b.toString ++ " count = " ++ c.toString) }

}


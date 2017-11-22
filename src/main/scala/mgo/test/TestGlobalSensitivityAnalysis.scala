/*
 * Copyright (C) Guillaume Chérel 17/10/2017
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
import mgo.contexts._
import freedsl.dsl._
import algorithm.sensitivityAnalysis.globalSensitivityAnalysis._
import util.Random

object GlobalSensitivityAnalysis extends App {

  implicit val rng = new Random()

  val nReplications = 50

  val k = 2

  def fTwoVarAdd(row: Vector[Double]) = row(0) + 0.5 * row(1)
  def fTwoVarAddInter(row: Vector[Double]) = row(0) + 0.5 * row(1) + row(0) * row(1)

  val sampleUnit = (0.0 to 1.0 by 0.0001).toVector

  // Matrices A and B by columns.
  def sampleMat(implicit rng: Random) = {
    val Acol = Vector.fill(k)(rng.shuffle(sampleUnit))
    val Bcol = Vector.fill(k)(rng.shuffle(sampleUnit))
    val Ccol = (1 to k).map(i =>
      Bcol.take(i - 1) ++ Vector(Acol(i - 1)) ++ Bcol.takeRight(k - i)
    ).toVector

    // Matrices by rows.
    val B = Bcol.transpose
    val C = Ccol.map(_.transpose)

    (B, C)
  }

  // Precomputed
  def replicationsFromPrecomputed(f: Vector[Double] => Double)(implicit rng: Random) =
    for { replications <- (0 to nReplications) } yield {
      val BC = sampleMat
      val B = BC._1
      val C = BC._2
      // Precompute output
      val fB = B.map(f.andThen(Option.apply))
      val fC = C.map(Ci => Ci.map(f.andThen(Option.apply)))

      val result = fromPrecomputed_Rn_R(fB, fC)
      result.run({ (i: Unit) => () })
    }

  def replicationsUniformInputs(f: Vector[Double] => Double)(implicit rng: Random) =
    for { replications <- (0 to nReplications) } yield {
      uniformInputs_Rn_R(
        minmax = Vector.fill(k)((-10.0, 4.0)),
        numberOfSamples = 1000
      ).run(fTwoVarAdd _)
    }

  println("fromPrecomputed VarAdd:\t" ++ replicationsFromPrecomputed(fTwoVarAdd _).transpose.map(x => x.sum / x.size).toString)
  println("uniformInputs VarAdd:\t" ++ replicationsFromPrecomputed(fTwoVarAdd _).transpose.map(x => x.sum / x.size).toString)
  println("fromPrecomputed VarAddInter:\t" ++ replicationsFromPrecomputed(fTwoVarAddInter _).transpose.map(x => x.sum / x.size).toString)
  println("uniformInputs VarAddInter:\t" ++ replicationsFromPrecomputed(fTwoVarAddInter _).transpose.map(x => x.sum / x.size).toString)

  println("Expected VarAddInter: " ++ Vector((7.0 / 36.0) / (40.0 / 144.0), (13.0 / 144.0) / (40.0 / 144.0)).toString)
}


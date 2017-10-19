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

  implicit val rng = new Random(45)

  object testTwoVarAdd {
    val sampleUnit = (0.0 to 1.0 by 0.0001).toVector

    val k = 2

    val res = for { replications <- (0 to 50) } yield {
      // Matrices A and B by columns.
      val Acol = Vector.fill(k)(rng.shuffle(sampleUnit))
      val Bcol = Vector.fill(k)(rng.shuffle(sampleUnit))
      val Ccol = (1 to k).map(i => Bcol.take(i - 1) ++ Vector(Acol(i - 1)) ++ Bcol.takeRight(k - i)).toVector

      // Matrices by rows.
      val B = Bcol.transpose
      val C = Ccol.map(_.transpose)

      // Precompute output
      val fB = B.map(row => row(0) + 0.5 * row(1))
      val fC = C.map(Ci => Ci.map(row => row(0) + 0.5 * row(1)))

      val result = fromPrecomputed_Rn_R(fB, fC)
      result.run({ (i: Unit) => () })
    }

    println("2VarAdd:\t" ++ res.transpose.map(x => x.sum / x.size).toString)

  }

  testTwoVarAdd

  object testTwoVarAddInter {
    val sampleUnit = (0.0 to 1.0 by 0.0001).toVector

    val k = 2

    val res = for { replications <- (0 to 50) } yield {
      // Matrices A and B by columns.
      val Acol = Vector.fill(k)(rng.shuffle(sampleUnit))
      val Bcol = Vector.fill(k)(rng.shuffle(sampleUnit))
      val Ccol = (1 to k).map(i => Bcol.take(i - 1) ++ Vector(Acol(i - 1)) ++ Bcol.takeRight(k - i)).toVector

      // Matrices by rows.
      val B = Bcol.transpose
      val C = Ccol.map(_.transpose)

      // Precompute output
      val fB = B.map(row => row(0) + 0.5 * row(1) + row(0) * row(1))
      val fC = C.map(Ci => Ci.map(row => row(0) + 0.5 * row(1) + row(0) * row(1)))

      val result = fromPrecomputed_Rn_R(fB, fC)
      result.run({ (i: Unit) => () })
    }

    println("2VarAddInter:\t" ++ res.transpose.map(x => x.sum / x.size).toString)
  }

  testTwoVarAddInter

}


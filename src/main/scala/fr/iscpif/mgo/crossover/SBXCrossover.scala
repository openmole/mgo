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

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Math._
import util.Random
import monocle.syntax._

/**
 * SBX RGA operator with Bounded Variable modification, see APPENDIX A p30 into :
 *
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7291&rep=rep1&type=pdf
 *
 * INPROCEEDINGS{Deb98anefficient,
 *   author = {Kalyanmoy Deb},
 *   title = {An Efficient Constraint Handling Method for Genetic Algorithms},
 *   booktitle = {Computer Methods in Applied Mechanics and Engineering},
 *   year = {1998},
 *   pages = {311--338}
 * }
 *
 * Notes : Deb implementation differs from NSGA2 he proposed on this site :
 * http://www.iitk.ac.in/kangal/codes.shtml
 *
 * Implementation based on http://repository.ias.ac.in/9415/1/318.pdf
 *
 */
object SBXCrossover {

  def apply(crossover: Crossover with GA)(distributionIndex: Double = 1.0): crossover.Crossover = {
    import crossover._
    (g1: G, g2: G, population: Population[G, P, F], archive: A, rng: Random) => {
      val (o1, o2) = SBXCrossover.crossOver(values.get(g1), values.get(g2), distributionIndex)(rng)
      assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
      Seq(g1 applyLens values set o1, g2 applyLens values set o2)
    }
  }

  def crossOver(g1: Seq[Double], g2: Seq[Double], distributionIndex: Double)(implicit rng: Random): (Seq[Double], Seq[Double]) = {

    val exponent = 1.0 / (distributionIndex + 1.0)

    def elementCrossOver(x0i: Double, x1i: Double)(implicit rng: Random): (Double, Double) = {
      val u = rng.nextDouble

      val bq =
        if (u <= 0.5) math.pow(2 * u, exponent)
        else math.pow(1.0 / (2.0 * (1.0 - u)), exponent)

      val lb = 0.0
      val ub = 1.0
      val x0 = clamp(x0i, lb, ub)
      val x1 = clamp(x1i, lb, ub)
      val newX0 = 0.5 * ((1.0 + bq) * x0 + (1.0 - bq) * x1)
      val newX1 = 0.5 * ((1.0 - bq) * x0 + (1.0 + bq) * x1)
      (newX0, newX1)
    }

    (g1 zip g2).map {
      case (g1e, g2e) => elementCrossOver(g1e, g2e)
    }.unzip

  }

}
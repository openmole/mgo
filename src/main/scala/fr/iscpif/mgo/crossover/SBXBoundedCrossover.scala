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

/**
 * SBX RGA operator with Bounded Variable modification, see APPENDIX A p30 into :
 *
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7291&rep=rep1&type=pdf
 *
 * @INPROCEEDINGS{Deb98anefficient,
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
 * Implementation based on http://www.moeaframework.org/
 *
 */
object SBXBoundedCrossover {

  def elementCrossOver(x0i: Double, x1i: Double, distributionIndex: Double)(implicit rng: Random): (Double, Double) = {
    val lb = 0.0
    val ub = 1.0
    val x0 = clamp(x0i, lb, ub)
    val x1 = clamp(x1i, lb, ub)

    val dx = Math.abs(x1 - x0)
    if (dx > epsilon) {
      var bl: Double = 0.0
      var bu: Double = 0.0

      if (x0 < x1) {
        bl = 1 + 2 * (x0 - lb) / dx
        bu = 1 + 2 * (ub - x1) / dx
      } else {
        bl = 1 + 2 * (x1 - lb) / dx
        bu = 1 + 2 * (ub - x0) / dx
      }

      //use symmetric distributions
      if (bl < bu) {
        bu = bl
      } else {
        bl = bu
      }

      val p_bl = 1 - 1 / (2 * Math.pow(bl, distributionIndex + 1))
      val p_bu = 1 - 1 / (2 * Math.pow(bu, distributionIndex + 1))
      val u = rng.nextDouble
      val u0 = u * p_bl
      val u1 = u * p_bu

      val b0 =
        if (u0 <= 0.5) Math.pow(2 * u0, 1 / (distributionIndex + 1))
        else Math.pow(0.5 / (1 - u0), 1 / (distributionIndex + 1))

      val b1 =
        if (u1 <= 0.5) Math.pow(2 * u1, 1 / (distributionIndex + 1))
        else Math.pow(0.5 / (1 - u1), 1 / (distributionIndex + 1))

      val res =
        if (x0 < x1) (0.5 * (x0 + x1 + b0 * (x0 - x1)), 0.5 * (x0 + x1 + b1 * (x1 - x0)))
        else (0.5 * (x0 + x1 + b1 * (x0 - x1)), 0.5 * (x0 + x1 + b0 * (x1 - x0)))

      if (rng.nextBoolean) res else res.swap
    } else (x0, x1)
  }

  def crossOver(g1: Seq[Double], g2: Seq[Double], crossoverRate: Double, distributionIndex: Double)(implicit rng: Random): (Seq[Double], Seq[Double]) =
    /** crossover probability */
    (g1 zip g2).map {
      case (g1e, g2e) =>
        if (rng.nextDouble <= crossoverRate) elementCrossOver(g1e, g2e, distributionIndex) else (g1e, g2e)
    }.unzip

}

trait SBXBoundedCrossover extends Crossover with GA with CrossoverRate {

  /** distribution index parameter of the algorithm */
  def distributionIndex: Double = 2

  override def crossover(g1: G, g2: G, population: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random) = {
    val (o1, o2) = sbxCrossover(g1, g2)
    Seq(o1, o2)
  }

  def sbxCrossover(g1: G, g2: G)(implicit rng: Random) = {
    val (o1, o2) = SBXBoundedCrossover.crossOver(fullGenome.get(g1), fullGenome.get(g2), crossoverRate, distributionIndex)
    assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
    (fullGenome.set(g1, o1), fullGenome.set(g2, o2))
  }

}

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
 * Implementation based on http://repository.ias.ac.in/9415/1/318.pdf
 *
 */
object SBXBoundedCrossover {

  def crossOver(g1: Seq[Double], g2: Seq[Double], distributionIndex: Double)(implicit rng: Random): (Seq[Double], Seq[Double]) = {

    def exponent = 1.0 / (distributionIndex + 1.0)

    lazy val bq =
      if (rng.nextBoolean) math.pow(2.0 * rng.nextDouble, exponent)
      else math.pow(1.0 / (2.0 * (1.0 - rng.nextDouble)), exponent)

    def elementCrossOver(x0i: Double, x1i: Double)(implicit rng: Random): (Double, Double) = {
      val lb = 0.0
      val ub = 1.0
      val x0 = clamp(x0i, lb, ub)
      val x1 = clamp(x1i, lb, ub)

      val dx = Math.abs(x1 - x0)
      if (dx > epsilon) {
        val newX0 = 0.5 * ((1.0 + bq) * x0 + (1.0 - bq) * x1)
        val newX1 = 0.5 * ((1.0 - bq) * x0 + (1.0 + bq) * x1)
        (newX0, newX1)
      } else (x0, x1)
    }

    (g1 zip g2).map {
      case (g1e, g2e) => elementCrossOver(g1e, g2e)
    }.unzip

  }

}

trait SBXBoundedCrossover extends Crossover with GA {

  /** distribution index parameter of the algorithm */
  def distributionIndex: Double = 1.0

  override def crossover(g1: G, g2: G, population: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random) = {
    val (o1, o2) = sbxCrossover(g1, g2)
    Seq(o1, o2)
  }

  def sbxCrossover(g1: G, g2: G)(implicit rng: Random) = {
    val (o1, o2) = SBXBoundedCrossover.crossOver(fullGenome.get(g1), fullGenome.get(g2), distributionIndex)
    assert(!o1.exists(_.isNaN) && !o2.exists(_.isNaN), s"$o1, $o2 from $g1, $g2")
    (fullGenome.set(g1, o1), fullGenome.set(g2, o2))
  }

}

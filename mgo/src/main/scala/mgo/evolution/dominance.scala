/*
 * Copyright (C) 2015 Romain Reuillon
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
package mgo.evolution

object dominance:
  /**
   * Dominance type between 2 solution
   */
  trait Dominance:
    def isDominated(p1: Seq[Double], p2: Seq[Double]): Boolean

  /**
   * A point dominates another if the other is not better on any objective
   */
  lazy val nonStrictDominance: Dominance =
    (p1: Seq[Double], p2: Seq[Double]) =>
        val dominated =
          import scala.util.boundary
          var equal = true
          boundary[Boolean]:
            for (g1, g2) <- p1 lazyZip p2
              do
                if g1 < g2 then boundary.break(false)
                if g1 != g2 then equal = false

            !equal

        dominated


  /**
   * A point dominates another if all its objective are better
   */
  lazy val strictDominance: Dominance = new Dominance:
    override def isDominated(p1: Seq[Double], p2: Seq[Double]): Boolean =
      (p1 zip p2).forall { case (g1, g2) => g2 < g1 }

  /**
   * A point is dominated if all its objectif are above another point in a range
   * of epsilon A.G. Hernández-Díaz, L.V. Santana-Quintero, C.A.C. Coello,  and
   * J.M. Luque,   "Pareto-adaptive epsilon-dominance",
   *  presented at Evolutionary Computation, 2007, pp.493-517.
   */
  def nonStrictEpsilonDominance(epsilons: Seq[Double]): Dominance = new Dominance:
    override def isDominated(p1: Seq[Double], p2: Seq[Double]): Boolean =
      !(p1 zip p2 zip epsilons).exists {
        case (((g1, g2), e)) => g2 > e + g1
      }

  def strictEpsilonDominance(epsilons: Seq[Double]): Dominance = new Dominance:
    override def isDominated(p1: Seq[Double], p2: Seq[Double]): Boolean =
      (p1 zip p2 zip epsilons).forall:
        case (((g1, g2), e)) => g1 > g2 + e


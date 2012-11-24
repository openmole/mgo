/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.dominance

import fr.iscpif.mgo._

/**
 * A point is dominated if all its objectif are above another point in a range
 * of epsilon A.G. Hernández-Díaz, L.V. Santana-Quintero, C.A.C. Coello,  and
 * J.M. Luque,   "Pareto-adaptive epsilon-dominance",
 *  presented at Evolutionary Computation, 2007, pp.493-517.
 */
trait EpsilonDominance extends Dominance {

  /** epsilons values, one for each element in the fitness */
  def epsilons: Seq[Double]

  def infiniteEpsilons: Iterator[Double] = epsilons.iterator ++ Iterator.continually(0.0)

  def isDominated(p1: Seq[Double], p2: Seq[Double]): Boolean =
    (p1.iterator zip p2.iterator zip infiniteEpsilons).forall {
      case (((g1, g2), e)) => g1 > e + g2
    }

}
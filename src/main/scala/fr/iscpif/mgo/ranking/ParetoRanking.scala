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

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import genome.G
import tools.Lazy

/**
 * Layer to compute the rank according to the number individual that dominate a
 * given individual.
 */
trait ParetoRanking extends Ranking with Dominance with G with F {

  type RANKED <: MGFitness

  override def rank(evaluated: Seq[RANKED]) = {
    evaluated.zipWithIndex.map {
      case (indiv, index) =>
        Lazy(evaluated.par.zipWithIndex.filter {
          case (_, index2) => index != index2
        }.count {
          case (indiv2, _) => isDominated(indiv.values, indiv2.values)
        })
    }
  }
}


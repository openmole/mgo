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

import scala.util.Random

object ParetoRanking {

  def paretoRanking(algorithm: Dominance)(values: Seq[Seq[Double]]) = {
    import algorithm._

    values.zipWithIndex.map {
      case (v1, index1) =>
        Lazy(
          if (v1.exists(_.isNaN)) Int.MaxValue
          else {
            values.zipWithIndex.filter {
              case (_, index2) => index1 != index2
            }.count {
              case (v2, _) => isDominated(v1, v2)
            }
          }
        )
    }
  }

}

/**
 * Layer to compute the rank according to the number of individuals that dominate a
 * given individual.
 */
trait ParetoRanking extends Ranking with Dominance with MG {
  override def rank(values: Population[G, P, F])(implicit rng: Random) = paretoRanking(values.toIndividuals.map(fitness))
  def paretoRanking(values: Seq[Seq[Double]]) = ParetoRanking.paretoRanking(this)(values)
}


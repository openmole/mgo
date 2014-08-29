/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import util.Random
import tools._

trait RankElitism <: Elitism with G with P with F with Mu with Ranking {

  override def computeElitism(oldGeneration: Population[G, P, F], offspring: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] = {
    val population = filter(oldGeneration ++ offspring)
    if (population.size < mu) population
    else {
      val ranks = rank(population)
      (population zip ranks).sortBy { case (_, r) => r() }.map(_._1).take(mu)
    }
  }

}

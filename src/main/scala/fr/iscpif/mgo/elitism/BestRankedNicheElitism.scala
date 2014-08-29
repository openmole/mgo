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

import scala.util.Random

trait BestRankedNicheElitism <: NicheElitism with Ranking with MGFitness {
  override def keep(population: Population[G, P, F])(implicit rng: Random): Population[G, P, F] = {
    val ranks = rank(population).map(_())
    val topRank = ranks.min
    val niche = (population zip ranks).filter { case (_, r) => r == topRank }.map { case (i, _) => i }
    niche
  }
}

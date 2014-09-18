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

package fr.iscpif.mgo.selection

import fr.iscpif.mgo._
import tools.Lazy
import util.Random

trait TournamentOnDiversity <: Tournament with Diversity {

  type Evaluation = Lazy[Double]
  def evaluation(population: Population[G, P, F], archive: A)(implicit rng: Random) = diversity(population)

  override def tournament(e1: IndividualEvaluation, e2: IndividualEvaluation)(implicit rng: Random) = {
    val (_, d1) = e1
    val (_, d2) = e2

    if (d1() < d2()) e2
    else if (d1() > d2()) e1
    else if (rng.nextBoolean) e1 else e2
  }
}

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

import fr.iscpif.mgo.archive.HitMapArchive
import fr.iscpif.mgo.{ Population }

import scala.util.Random

trait TournamentOnHitCount <: Tournament with HitMapArchive {
  override type Evaluation = Int

  override def evaluate(population: Population[G, P, F], archive: A)(implicit rng: Random) = population.map(i => hits(archive, niche(i.toIndividual)))

  override def tournament(e1: IndividualEvaluation, e2: IndividualEvaluation)(implicit rng: Random) = {
    val (_, h1) = e1
    val (_, h2) = e2

    if (h1 < h2) e1
    else if (h2 < h1) e2
    else if (rng.nextBoolean) e1 else e2
  }

}

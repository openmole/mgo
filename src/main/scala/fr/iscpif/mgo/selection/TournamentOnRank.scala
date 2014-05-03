/*
 * Copyright (C) 09/02/14 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.selection

import scala.util.Random
import fr.iscpif.mgo._

trait TournamentOnRank <: Tournament with RankModifier {
  override def tournament(e1: PopulationElement[G, P, F, MF], e2: PopulationElement[G, P, F, MF])(implicit rng: Random): PopulationElement[G, P, F, MF] =
    if (rank.get(e1.metaFitness)() < rank.get(e2.metaFitness)()) e1
    else if (rank.get(e1.metaFitness)() > rank.get(e2.metaFitness)()) e2
    else if (rng.nextDouble < 0.5) e1 else e2

}

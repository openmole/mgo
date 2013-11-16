/*
 * Copyright (C) 13/11/13 Romain Reuillon
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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.distance.EuclideanDistance
import fr.iscpif.mgo.tools.Neighbours
import fr.iscpif.mgo.metric.CrowdingDistance

trait NoveltyModifier <: RankDiversityModifier
    with NoveltyArchive
    with EuclideanDistance
    with Neighbours
    with ParetoRanking {

  //def neighbours: Int

  override def fitnesses(evaluated: Seq[Individual[G, P, F]], archive: A) = {
    //def indivDistance(i1: Individual[G, P, F], i2: Individual[G, P, F]) = distance(i1.genome.values, i2.genome.values)
    val diversities = CrowdingDistance(evaluated.map(_.genome.values)).map(1.0 / _())
    (evaluated zip diversities).map { case (i, d) => i.fitness.values ++ Seq(d) }
  }

}

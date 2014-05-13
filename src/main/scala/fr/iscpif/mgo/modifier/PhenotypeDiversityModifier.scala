/*
 * Copyright (C) 12/05/14 Guillaume Chérel
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
import fr.iscpif.mgo._
import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.tools.distance.EuclideanDistance
import fr.iscpif.mgo.tools.Neighbours
import fr.iscpif.mgo.tools.{ Lazy, Neighbours }
import fr.iscpif.mgo.metric.CrowdingDistance
import scalaz.Lens

trait PhenotypeDiversityModifier <: RankDiversityModifier
    with Archive
    with Neighbours
    with ParetoRanking {

  override type P <: Seq[Double]

  override def modify(evaluated: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] = {
    val f = fitnesses(evaluated, archive)
    val ranks = rank(f)
    val distances = diversity(evaluated.map(_.phenotype), ranks)
    RankDiversityModifier.toPopulationElements[G, P, F](evaluated, ranks, distances)
  }
}

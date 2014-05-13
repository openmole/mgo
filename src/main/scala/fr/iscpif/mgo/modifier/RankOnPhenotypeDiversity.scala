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

trait RankOnPhenotypeDiversity <: ModifiedFitness with DiversityMetric with DoubleSeqPhenotype with HierarchicalRanking {

  override def fitnesses(evaluated: Seq[Individual[G, P, F]], archive: A) =
    diversity(evaluated.map(i => doubleSeq.get(i.phenotype))).map(d => Seq(1 / d()))

}

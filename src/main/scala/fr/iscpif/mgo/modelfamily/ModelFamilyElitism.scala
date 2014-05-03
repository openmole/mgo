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

package fr.iscpif.mgo.modelfamily

import fr.iscpif.mgo._
import scala.util.Random
import fr.iscpif.mgo.elitism.{ MergedGenerations, Elitism }

trait ModelFamilyElitism <: Elitism with Aggregation with MergedGenerations with ModelFamilyGenome {

  def nicheSize: Int

  def niches(individuals: Seq[Individual[G, P, F]]) = individuals.groupBy(i => modelId.get(i.genome)).toSeq

  override def elitism(individuals: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random): Seq[Individual[G, P, F]] =
    niches(individuals).map {
      case (_, is) => is.sortBy(i => aggregate(i.fitness)).take(nicheSize)
    }.flatten

}

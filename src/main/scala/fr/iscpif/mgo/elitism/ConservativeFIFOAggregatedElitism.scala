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
import tools._

import scala.util.Random

trait ConservativeFIFOAggregatedElitism <: Elitism with Aggregation with Mu {
  def conservativeFIFO(oldGeneration: Population[G, P, F], candidate: PopulationElement[G, P, F])(implicit rng: Random): Population[G, P, F] =
    if (oldGeneration.size < 2) oldGeneration ++ Seq(candidate)
    else {
      val oldests = oldGeneration.zipWithIndex.groupBy { case (i, _) => i.age }.toSeq.sortBy { case (age, _) => age }.reverse.head._2
      val (oldest, oldestIndex) = oldests.sortBy { case (i, _) => aggregate(i.fitness) }.head

      val (concurrent, concurrentIndex) = oldGeneration.zipWithIndex.patch(oldestIndex, Seq.empty, 1).random
      if (aggregate(oldest.fitness) <= aggregate(concurrent.fitness)) oldGeneration.updated(concurrentIndex, candidate)
      else oldGeneration.updated(oldestIndex, candidate)
    }

  override def computeElitism(oldGeneration: Population[G, P, F], offspring: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] =
    if (oldGeneration.size < mu) oldGeneration ++ offspring
    else offspring.foldLeft(oldGeneration)(conservativeFIFO)

}

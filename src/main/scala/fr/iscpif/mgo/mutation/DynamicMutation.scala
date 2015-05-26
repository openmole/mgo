/*
 * Copyright (C) 2015 Romain Reuillon
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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import tools._
import monocle.syntax._
import monocle._

import scala.util.Random

trait DynamicMutation <: Mutation {
  def mutationExploration: Double = 0.1
  def fromMutation: Lens[G, Option[Int]]

  def mutationStats(p: Population[G, P, F]): collection.Map[Mutation, Double] = {
    val working = p.flatMap(_.genome &|-> fromMutation get)
    val map = working.groupBy(identity).mapValues(_.size.toDouble / working.size)
    (0 until mutations.size).map(i => mutations(i) -> map.getOrElse(i, 0.0)).toMap
  }

  override def mutate(genome: G, population: Population[G, P, F], archive: A)(implicit rng: Random): G = {
    val mutation: Mutation =
      if (rng.nextDouble < mutationExploration) mutations.random
      else multinomial(mutationStats(population))
    mutation(genome, population, archive, rng)
  }

}

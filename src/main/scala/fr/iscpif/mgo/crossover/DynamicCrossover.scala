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

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo.Population
import fr.iscpif.mgo.tools._
import monocle._
import monocle.syntax._

import scala.util.Random

trait DynamicCrossover <: Crossover {

  def crossoverExploration: Double = 0.1
  def fromCrossover: Lens[G, Option[Int]]

  def crossoverStats(p: Population[G, P, F]): collection.Map[Crossover, Double] = {
    val working = p.flatMap(_.genome &|-> fromCrossover get)
    val map = working.groupBy(identity).mapValues(_.size.toDouble / working.size)
    (0 until crossovers.size).map(i => crossovers(i) -> map.getOrElse(i, 0.0)).toMap
  }

  override def crossover(g1: G, g2: G, population: Population[G, P, F], archive: A)(implicit rng: Random): Seq[G] = {
    val crossover: Crossover =
      if (rng.nextDouble < crossoverExploration) crossovers.random
      else mutltinomial(crossoverStats(population))
    crossover(g1, g2, population, archive, rng)
  }

}

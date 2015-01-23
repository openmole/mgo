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

package fr.iscpif.mgo.ranking

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.KDTree
import fr.iscpif.mgo.tools.metric.CrowdingDistance

import scala.util.Random

trait CeilDiversityRanking <: ParetoRanking with GA with MG with Aggregation {

  def ceil: Double

  override def rank(p: Population[G, P, F])(implicit rng: Random) = {
    def genomes = p.map(i => values.get(i.genome))
    def diversityObjective: Seq[Double] = CrowdingDistance(genomes).map(c => 1 / c())

    def fitnesses =
      for {
        f <- p.toIndividuals.map(_.fitness)
        a = aggregate(f)
      } yield {
        if (a < ceil) 0.0
        else a
      }

    def fitnessAndCrowding: Seq[Seq[Double]] = (fitnesses zip diversityObjective).map { case (f, c) => Seq(f, c) }
    paretoRanking(fitnessAndCrowding)
  }

}

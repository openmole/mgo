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

trait DiversityRanking <: ParetoRanking with GA with MG {

  def neighbours = 10

  override def rank(p: Population[G, P, F])(implicit rng: Random) = {
    val genomes = p.map(i => values.get(i.genome))
    val tree = KDTree(genomes)

    def distance(g: Seq[Double]) =
      tree.knearest(neighbours, g).map { n => tree.distance(g, n) }.sum

    def diversityObjective = genomes.map(distance).map(1 / _)

    def fitnessAndCrowding: Seq[Seq[Double]] = (p zip diversityObjective).map { case (i, c) => fitness(i.fitness) ++ Seq(c) }
    paretoRanking(fitnessAndCrowding)
  }

}

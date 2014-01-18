/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.metric._
import RankDiversityModifier._
/**
 * Decorate the inidividual with a rank and a diversity. The rank is evaluated on
 * the fitness with an additionnal objective of genomic diversity 1 / crowding distance
 * of the genome.
 */
trait RankDiversityGenomicCrowdingModifier <: Modifier
    with RankModifier
    with DiversityModifier
    with Ranking
    with DiversityMetric
    with RankDiversity
    with GA {

  type F <: MGFitness

  override def modify(evaluated: Seq[Individual[G, P, F]], archive: A) = {
    val genomeDiversity = CrowdingDistance(evaluated.map { i => values.get(i.genome) })

    val diversityFitnesses =
      (evaluated zip genomeDiversity).map {
        case (i, gd) => i.fitness.values.toList ::: 1 / gd() :: Nil
      }

    val ranks = rank(diversityFitnesses)
    val distances = diversity(diversityFitnesses, ranks)

    toPopulationElements[G, P, F](evaluated, ranks, distances)
  }

}

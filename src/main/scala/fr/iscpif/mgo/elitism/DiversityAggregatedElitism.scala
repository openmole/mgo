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

import scala.util.Random
import tools.SeqDecorator

/**
 * Based on http://sci2s.ugr.es/publications/ficheros/2005-SC-Lozano.pdf
 */
trait DiversityAggregatedElitism <: Elitism with ConservativeFIFOAggregatedElitism with Aggregation with Mu with GA {

  def averageIntervalUsage(individuals: Seq[Individual[G, P, F]]): Double = {
    val transposedGenomes = individuals.map(i => values.get(i.genome)).transpose
    val mins = transposedGenomes.map(_.min)
    val maxs = transposedGenomes.map(_.max)
    (maxs zip mins).map { case (max, min) => max - min }.sum / genomeSize
  }

  override def elitism(oldGeneration: Seq[Individual[G, P, F]], offspring: Seq[Individual[G, P, F]], archive: A)(implicit rng: Random): Seq[Individual[G, P, F]] =
    if (oldGeneration.size < mu) oldGeneration ++ offspring
    else offspring.foldLeft(oldGeneration) {
      (individuals, candidate) =>
        val worsts =
          individuals.zipWithIndex.filter {
            case (i, _) => aggregate(i.fitness) > aggregate(candidate.fitness)
          }

        lazy val individualsAIU = averageIntervalUsage(individuals)
        lazy val candidateAIUContribution = averageIntervalUsage(Seq(candidate) ++ individuals) - individualsAIU

        val smallerAIU: Seq[Int] =
          worsts.map {
            case e @ (_, index) => e -> (individualsAIU - averageIntervalUsage(oldGeneration.patch(index, Seq.empty, 1)))
          }.filter {
            case (_, aiuContribution) => aiuContribution < candidateAIUContribution
          }.sortBy {
            case (_, aiuContribution) => aiuContribution
          }.map {
            case ((_, index), _) => index
          }

        if (!smallerAIU.isEmpty) individuals.updated(smallerAIU.head, candidate)
        else conservativeFIFO(individuals, candidate)
    }

}

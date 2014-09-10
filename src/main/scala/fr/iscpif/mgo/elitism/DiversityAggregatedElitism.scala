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
trait DiversityAggregatedElitism <: Elitism with Aggregation with Mu with GA {

  def averageIntervalUsage(individuals: Population[G, P, F]): Double = {
    val transposedGenomes = individuals.map(i => values.get(i.genome)).transpose
    val mins = transposedGenomes.map(_.min)
    val maxs = transposedGenomes.map(_.max)
    (maxs zip mins).map { case (max, min) => max - min }.sum / genomeSize
  }

  override def computeElitism(oldGeneration: Population[G, P, F], offspring: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] =
    offspring.foldLeft(oldGeneration) {
      (population, candidate) =>
        if (population.size + 1 < mu) filter(population ++ Seq(candidate))
        else {
          val worsts =
            population.zipWithIndex.filter {
              case (i, _) => aggregate(i.fitness) >= aggregate(candidate.fitness)
            }

          if (worsts.isEmpty) population
          else {
            lazy val populationAIU = averageIntervalUsage(population)

            val (worstIndex, aiuDiff) =
              worsts.map {
                case (_, index) =>
                  val modifiedPopulation = population.patch(index, Seq.empty, 1)
                  val modifiedPopulationAIU = averageIntervalUsage(modifiedPopulation)
                  val candidateAIUContribution = averageIntervalUsage(Seq(candidate) ++ modifiedPopulation) - modifiedPopulationAIU
                  val elementContribution = populationAIU - modifiedPopulationAIU
                  index -> (candidateAIUContribution - elementContribution)
              }.maxBy {
                _._2
              }

            filter(
              if (aiuDiff > 0) population.updated(worstIndex, candidate)
              else population.sortBy(i => aggregate(i.fitness)).reverse.tail.toList ++ Seq(candidate)
            )
          }
        }
    }

}

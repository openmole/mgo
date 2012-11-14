/*
 * Copyright (C) 2012 reuillon
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

package fr.iscpif.mgo.algorithm

import java.util.Random

import fr.iscpif.mgo._

/**
 * Implementation of the NSGAII algorithm Deb, K., Agrawal, S., Pratap, A. & 
 * Meyarivan, T. A fast elitist non-dominated sorting genetic algorithm for 
 * multi-objective optimization: NSGA-II. Lecture notes in computer science 
 * 1917, 849–858 (2000).
 * 
 */
trait NSGAII extends genome.GAEvolution with Mu with Elitism with Breeding with DiversityMetric {

  type G <: GAGenome
  type F = MGFitness

  override def evolve(population: Population[G, F, MF], evaluator: G => F)(implicit aprng: Random): Population[G, F, MF] = {
    val offspring = breed(
      population
    ).par.map { g => Individual(g, evaluator) }

    val archive = population.toIndividuals ++ offspring

    //Elitism strategy
    val individuals = toPopulation(archive)
    elitism(individuals)
  }

}

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
import scala.annotation.tailrec

trait NSGAII extends GAEvolution with MG with Archive with Elitism with Breeding with DiversityMetric {

  
  override def evolve(population: Population[G, MF], evaluator: G => Fitness)(implicit aprng: Random): Population[G, MF] = {
    val offspring = breed(
      population
    ).par.map { g => Individual(g, evaluator) }

    val archive = population.individuals ++ offspring

    //Elitisme strategy
    val individuals = toPopulation(archive)
    elitism(individuals)
  }



}

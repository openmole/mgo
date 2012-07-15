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
import fr.iscpif.mgo.crossover._
import fr.iscpif.mgo.elitism._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.ranking._
import fr.iscpif.mgo.diversity._
import fr.iscpif.mgo.selection._
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo._
import fr.iscpif.mgo.breed._
import fr.iscpif.mgo.termination._
import fr.iscpif.mgo.tools.Math
import ga._
import scala.annotation.tailrec



// WORK NOTE (A ne pas effacer) :
/*
 Selection occurs two times in the evolutionary loop.First, in order to generate offsprings,
 parents must be selected from the current population (mating selection).Second, the new
 parent population has to be selected from the offspring and the previous parents (Environmental selection)
 A number of selection operators were proposed, which usually base the chance of selection of
 particular individuals on their fitness values or their rank in the population, respectively.

 // Environmental selection
 // How to prevent non-dominated solutions from being lost?
 // Environmental selection is used to obtain a representative efficient set

 */

// @fixme Refaire un check sur Ranking

trait NSGAII extends Evolution with MG with Archive with Elitism with Breeding with DiversityMetric {

  
  override def evolve(population: Population[G, MF], evaluator: G => Fitness)(implicit aprng: Random): Population[G, MF] = {
    val offspring = breed(
      population,
      population.size
    ).par.map { Individual(_, evaluator) }

    val archive = population.individuals ++ offspring

    //Elitisme strategy
    val individuals = toPopulation(archive)
    elitism(individuals)
  }



}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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

  //type I = Individual[G] with Diversity with Rank
  
  def archiveSize: Int

  override def evolve(population: Population[G, MF], evaluator: G => Fitness)(implicit aprng: Random, factory: Factory[G]): Population[G, MF] = {
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

/*
 * Copyright (C) 2015 Guillaume Chérel
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

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import scala.util.Random
import math.{ round, max }
import collection.immutable.Map

/**
 * Cake layer to eliminated elements of a population
 */
trait NEATElitism <: Elitism with DoubleFitness with NEATGenome with NEATArchive {
  def proportionKeep: Double // = 0.2

  /**
   * Keep only the 20% fittest individuals of each species. If the fitness of the entire population does not
   * improve for more than 20 generations, only the two best species are allowed to reproduce.
   */
  def computeElitism(oldGeneration: Population[G, P, F], offsprings: Population[G, P, F], archive: A)(implicit rng: Random): Population[G, P, F] = {
    val indivsBySpecies: Map[Int, Seq[PopulationElement[G, P, F]]] =
      offsprings.content.groupBy { elt => elt.genome.species }
    //If the fitness of the entire population does not improve for more than 20 generations
    val lastfitnesses = archive.lastEntirePopulationFitnesses.takeRight(19)
    if ((lastfitnesses.length >= 20) &&
      lastfitnesses.foldLeft(
        (true, lastfitnesses.head)
      ) { (acc, curf) =>
          val (notbetter, prevfitness) = acc
          (notbetter && (prevfitness >= curf), curf)
        }._1) {
      // Only allow the 2 best species to reproduce
      val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map { case (sp, indivs) => (sp, indivs.map { _.fitness }.sum / indivs.size) }.toSeq
      val twoBest = speciesFitnesses.sortBy { case (sp, f) => -f }.take(2).map { _._1 }
      twoBest.flatMap { sp => keepBest(indivsBySpecies(sp)) }
    } else {
      indivsBySpecies.toSeq.flatMap { case (sp, indivs) => keepBest(indivs) }
    }
  }

  def keepBest(offsprings: Seq[PopulationElement[G, P, F]]): Seq[PopulationElement[G, P, F]] = {
    //keep only a portion of the offsprings, but take at least one.
    val keep = offsprings.sortBy { elt: PopulationElement[G, P, F] => -(elt.fitness) }.take(max(1, round(proportionKeep * offsprings.length).toInt))
    keep
  }
}

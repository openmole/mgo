/*
 * Copyright (C) 17/09/2015 Guillaume Chérel
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
package fr.iscpif.mgo.selection

import fr.iscpif.mgo.breed.BreedingContext
import fr.iscpif.mgo.fitness.F
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.phenotype.P
import fr.iscpif.mgo.{ Lambda, Individual, Population }

import scalaz._
import Scalaz._

import scala.util.Random

trait NEATMating <: Mating with Lambda with BreedingContext with NEATGenome with P with F {

  override def mate(population: Population[G, P, F], archive: A)(implicit rng: Random): Iterator[BreedingContext[Vector[Individual[G, P, F]]]] =
    selection(population, archive, lambda).map { couple => Vector(couple._1, couple._2).point[BreedingContext] }

  /** returns pairs of parents from the population */
  def selection(
    population: Population[G, P, F],
    archive: A,
    size: Int)(implicit rng: Random): Iterator[(Individual[G, P, F], Individual[G, P, F])] = {
    val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
      population.toIndividuals.groupBy { indiv => indiv.genome.species }
    val result = speciesOffsprings(indivsBySpecies, size).flatMap {
      case (species, nb) =>
        Iterator.fill(nb) {
          val nparents = indivsBySpecies(species).length
          val p1 = indivsBySpecies(species)(rng.nextInt(nparents))
          val p2 =
            // Have a chance to mate between different species
            if ((rng.nextDouble() < interSpeciesMatingProb) && (indivsBySpecies.size > 1)) {
              val otherSpecies = indivsBySpecies.keys.filter { _ != species }.toSeq(rng.nextInt(indivsBySpecies.size - 1))
              val nindivsOtherSpecies = indivsBySpecies(otherSpecies).length
              indivsBySpecies(otherSpecies)(rng.nextInt(nindivsOtherSpecies))
            } else indivsBySpecies(species)(rng.nextInt(nparents))
          //println(s"sampling species (${p1.genome.species}, ${p2.genome.species})")
          (p1, p2)
        }
    }

    result.toIterator
  }

  /** Returns tuples (species, number of offsprings) */
  def speciesOffsprings(
    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
    totalOffsprings: Int): Vector[(Int, Int)]

  def interSpeciesMatingProb: Double

}

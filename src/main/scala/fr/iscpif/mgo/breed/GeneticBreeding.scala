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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import fr.iscpif.mgo.selection.Mating
import util.Random
import fr.iscpif.mgo.genome.RandomGenome
import fr.iscpif.mgo.tools._
import scalaz._
import Scalaz._

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait GeneticBreeding <: Breeding with G with F with P with Mating with Cloning with Crossover with Mutation with RandomGenome {

  /**
   * Breed genomes from a population
   *
   * @param population the population from which genomes are breeded
   * @param size the size of the breeded set
   * @return the breeded genomes
   */
  def breed[B[_]: Monad](population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Vector[G] = {
    val offsprings: Iterator[B[Vector[G]]] =
      if (population.isEmpty) Iterator.continually(randomGenome.pure[B])
      else {
        val breeded: Iterator[B[Vector[G]]] =
          // bind each result of mate to a breed action
          mate[B](population, archive).map { (_: B[Vector[Individual[G, P, F]]]) >>= { breed[B](_, population, archive) } }

        val cloned: Iterator[B[G]] =
          Iterator.continually(clone[B](population.toIndividuals.random))

        // breed or clone
        Iterator.continually {
          if (rng.nextDouble >= cloneProbability)
            breeded.next
          else
            cloned.next.map { Vector(_) }
        }
      }

    extractBreedingResult[B](
      // Get the B in the outer position with sequence, then flatten the inner nested vectors with join
      (offsprings.take(size).toVector: Vector[B[Vector[G]]]).sequence[B, Vector[G]].map { (_: Vector[Vector[G]]).join })
  }

  def breed[B[_]: Monad](
    individuals: Vector[Individual[G, P, F]],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): B[Vector[G]] =
    /*implicitly[Monad[B]].bind[Vector[G], Vector[G]](
      crossover[B](individuals.map { _.genome }, population, archive))(
        parents => {
          implicitly[Monad[B]].map {
            implicitly[Traverse[Vector]].traverse[B, G, G](parents.toVector) { mutate[B](_, population, archive) }
          } { _.toVector }
        })
    */
    crossover[B](individuals.map { _.genome }, population, archive) >>= { parents =>
      parents.traverse[B, G] { mutate[B](_, population, archive) }
    }

  /** extract the content from the breeding monad */
  def extractBreedingResult[B[_]: Monad](x: B[Vector[G]]): Vector[G]
}

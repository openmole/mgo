/*
 * Copyright (C) 2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.NEATGenome

import scala.util.Random
import scala.collection.immutable.Queue
import collection.immutable.IntMap

object NEATArchive {
  case class Archive(
    // to maintain a record of innovation throughout generations would require to make the whole Evolution stateful
    // so that innovations created at the breeding stage can be added. Let's just record the innovations for the 
    // current generation at the breeding stage only (like in Stanley's original paper).
    //recordOfInnovations: Seq[NEATGenome.Innovation],
    indexOfSpecies: IntMap[NEATGenome.Genome],
    lastEntirePopulationFitnesses: Queue[Double])
}

trait NEATArchive extends Archive with NEATGenome with DoubleFitness {

  type A = NEATArchive.Archive

  def initialArchive(implicit rng: Random): A =
    NEATArchive.Archive(
      //0,
      //0,
      //Vector[NEATGenome.Innovation](),
      IntMap[G](),
      Queue[Double]())

  def archive(a: A, oldIndividuals: Population[G, P, F], offsprings: Population[G, P, F])(implicit rng: Random): A =
    NEATArchive.Archive(
      //globalInnovationNumber = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation.number }.max,
      /* recordOfInnovation contains the unique innovations of offsprings*/
      //recordOfInnovations = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation }.distinct,
      /** The index of species represents each species by a random genome of the corresponding species of the past generation.*/
      indexOfSpecies = IntMap.empty ++ offsprings.toIndividuals.map { _.genome }.groupBy { g => g.species }.map { case (sp, indivs) => (sp, indivs(rng.nextInt(indivs.length))) },
      lastEntirePopulationFitnesses =
        a.lastEntirePopulationFitnesses.enqueue(offsprings.content.foldLeft(0.0) { (sum: Double, b: PopulationElement[G, P, F]) =>
          sum + (b.fitness)
        } / offsprings.content.length.toDouble)
    )
}

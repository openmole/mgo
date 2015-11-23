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

//package fr.iscpif.mgo.archive

//import fr.iscpif.mgo._
//import fr.iscpif.mgo.genome.NEATGenome
//
//import scala.util.Random
//import scala.collection.immutable.Queue
//import collection.immutable.IntMap
//import math._

/*trait NEATArchive extends Archive with NEATGenome with DoubleFitness {

  case class Archive(
    // to maintain a record of innovation throughout generations would require to make the whole Evolution stateful
    // so that innovations created at the breeding stage can be added. Let's just record the innovations for the
    // current generation at the breeding stage only (like in Stanley's original paper).
    //recordOfInnovations: Seq[NEATGenome.Innovation],
    indexOfSpecies: IntMap[G],
    lastEntirePopulationFitnesses: Queue[Double],
    speciesCompatibilityThreshold: List[Double])

  type A = Archive

  def numberSpeciesTarget: Int
  def speciesCompatibilityThreshold: Double
  def speciesCompatibilityMod: Double
  def speciesCompatibilityMin: Double

  def initialArchive(implicit rng: Random): A =
    Archive(
      //0,
      //0,
      //Vector[NEATGenome.Innovation](),
      IntMap[G](),
      Queue[Double](),
      List[Double](speciesCompatibilityThreshold))

  def archive(a: A, oldIndividuals: Population[G, P, F], offsprings: Population[G, P, F])(implicit rng: Random): A = {
    val indivsBySpecies: IntMap[Seq[G]] = IntMap.empty ++ offsprings.toIndividuals.map { _.genome }.groupBy { g => g.species }
    val newios: IntMap[G] =
      indivsBySpecies.map { case (sp, indivs) => (sp, indivs(rng.nextInt(indivs.length))) }
    val numberOfSpecies = newios.size
    val lastsct = a.speciesCompatibilityThreshold.head
    val newsct =
      max(
        speciesCompatibilityMin,
        if (numberOfSpecies < numberSpeciesTarget)
          lastsct - speciesCompatibilityMod
        else lastsct + speciesCompatibilityMod)
    Archive(
      //globalInnovationNumber = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation.number }.max,
      /* recordOfInnovation contains the unique innovations of offsprings*/
      //recordOfInnovations = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation }.distinct,
      /** The index of species represents each species by a random genome of the corresponding species of the past generation. */
      indexOfSpecies = newios,
      lastEntirePopulationFitnesses =
        a.lastEntirePopulationFitnesses.enqueue(offsprings.content.map {
          _.fitness
        }.sum / offsprings.content.size),
      newsct :: a.speciesCompatibilityThreshold
    )
  }
}*/


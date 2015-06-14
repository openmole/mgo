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

object NEATArchive {
  case class Archive(
    globalInnovationNumber: Int,
    recordOfInnovations: Seq[NEATGenome.NumberedInnovation],
    indexOfSpecies: Seq[NEATGenome.Genome[NEATGenome.NumberedInnovation]],
    lastEntirePopulationFitnesses: Seq[Double])
}

trait NEATArchive extends Archive {

  type A = NEATArchive.Archive

  def initialArchive(implicit rng: Random): A =
    NEATArchive.Archive(
      0,
      Vector[NEATGenome.NumberedInnovation](),
      Vector[NEATGenome.Genome[NEATGenome.NumberedInnovation]](),
      Queue[Double]())

  def archive(a: A, oldIndividuals: Population[NEATGenome.Genome[NEATGenome.Innovation], P, F], offsprings: Population[NEATGenome.Genome[NEATGenome.Innovation], P, F])(implicit rng: Random): A =
    NEATArchive.Archive(
      globalInnovationNumber = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation match { case (i: NEATGenome.NumberedInnovation) => i.number; case _ => 0 } }.max,
      /*recordOfInnovation contains the unique innovations of offsprings*/
      recordOfInnovations = offsprings.content.flatMap { _.genome.connectionGenes }.map { _.innovation }.distinct,
      indexOfSpecies = ???,
      lastEntirePopulationFitnesses = ???)
}

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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import util.Random
import fr.iscpif.mgo.genome.RandomGenome
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.archive.NEATArchive

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait NEATBreeding <: GeneticBreeding {

  /**
   * Sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found it the archive's record of innovations are attributed its number
   */
  override def postBreeding(population: Population[NEATGenome.Genome[NEATGenome.Innovation], P, F], offsprings: Seq[NEATGenome.Genome[NEATGenome.Innovation]], archive: NEATArchive.Archive)(implicit rng: Random): Seq[NEATGenome.Genome[NEATGenome.NumberedInnovation]] = {
    val (newgenomes, newgin, newroi) = offsprings.foldLeft((Seq[NEATGenome.Genome[NEATGenome.NumberedInnovation]](), archive.globalInnovationNumber, archive.recordOfInnovations)) { (acc, genome) =>
      val (curgenomes, curgin, curroi) = acc
      val (newgenome, newgin, newroi) = genome.setInnovationNumber(curgin, curroi)
      (curgenomes :+ newgenome, newgin, newroi)
    }
    newgenomes
  }

}

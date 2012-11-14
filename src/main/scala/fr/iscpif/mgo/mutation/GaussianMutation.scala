/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.GenomeFactory
import fr.iscpif.mgo.tools.Random._
import fr.iscpif.mgo.tools.Math._
import java.util.Random

/**
 * Mutation of a genome based on gausian distribution arrount the genome with 
 * fixed sigma values.
 */
trait GaussianMutation extends Mutation with GenomeFactory {

  type G <: genome.GAGenome

  /** sigma values, one for each element in the rest of the genome */
  def sigma : Double

  override def mutate (genome: G) (implicit aprng: Random) : G = {
    val newValues = genome.values map {
      v => 
        clamp (v + (aprng.nextGaussian * sigma), 0, 1)
 
    }
    genomeFactory(genome.updatedValues(newValues))
  }  
}
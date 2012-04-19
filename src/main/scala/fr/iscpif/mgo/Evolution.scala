/*
 * Copyright (C) 2011 srey
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo

import annotation.tailrec
import fr.iscpif.mgo.crossover.CrossOver
import fr.iscpif.mgo._
import fr.iscpif.mgo.mutation.Mutation
import fr.iscpif.mgo.selection.Selection
import fr.iscpif.mgo.termination.Termination
import java.util.Random

trait Evolution extends Mutation with CrossOver with Termination with Selection { self =>

  type I <: Individual[G, FIT]
  type G <: Genome
  type F <: GenomeFactory[G]
  type FIT <: Fitness
  
  def factory: F
  def evaluator: G => FIT

  //Perform N step
  @tailrec private def evolveStep(population: IndexedSeq[I], step: Int = 0)(implicit aprng:Random):IndexedSeq[I]= {
    val nextPop = evolve(population)
    if (terminated(population, nextPop, step)) nextPop
    else evolveStep(nextPop, step + 1)
  }

  def evolveRun(population: IndexedSeq[I], step: Int = 0) (implicit aprng: Random): IndexedSeq[I] = evolveStep(population,step)

  def evolve(population: IndexedSeq[I])(implicit aprng: Random): IndexedSeq[I]


}

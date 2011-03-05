/*
 * Copyright (C) 2011 reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.asrea

import org.openmole.tools.mgo.model.Individual
import org.openmole.tools.mgo.domination.DominateMinimization
import org.openmole.tools.mgo.domination.diversity.Crowding._
import org.openmole.tools.mgo.model.MultiGoalLike
import org.openmole.tools.mgo.model.Population
import java.util.Random


class Archive[I <: Individual[_,_]](individuals: Array[I]) extends Population[I](individuals) {

  implicit def individualAndIndexToMultiGoalLike(elt: (I, Int)) = new MultiGoalLike{override def goals = elt._1.goals; def index = elt._2}

  def +=(individual: I)(implicit rng: Random): this.type = {
    val dominatedIndices = dominatesInTheAchive(individual)
    val indivIsDominated = isDominatedInTheAchive(individual)
    
    if(!dominatedIndices.isEmpty && !indivIsDominated) {
      individuals(dominatedIndices(rng.nextInt(dominatedIndices.size))) = individual
    } else { 
      if(!indivIsDominated) {
        val indice = orderByDecreasingCrowding(individuals.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}).last._1.index
        individuals(indice) = individual
      }
    }
    this
  }
  
  def ranks(individual: I) = individuals.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}.filter(DominateMinimization.isDominated(individual, _)).map{_.index}.size

  private def dominatesInTheAchive(individual: I) = individuals.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}.filter(DominateMinimization.isDominated(_, individual)).map{_.index}
  private def isDominatedInTheAchive(individual: I) = individuals.exists(DominateMinimization.isDominated(individual, _))
  
}

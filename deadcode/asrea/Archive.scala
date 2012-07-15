/*
 * Copyright (C) 2011 Romain Reuillon
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

package fr.iscpif.mgo.asrea

/*import fr.iscpif.mgo.model.Individual
import fr.iscpif.mgo.domination.DominateMinimization
import fr.iscpif.mgo.domination.diversity.Crowding._
import fr.iscpif.mgo.model.MultiGoalLike
import fr.iscpif.mgo.model.Population
import java.util.Random
import scala.collection.mutable.ArraySeq


class Archive[I <: Individual[_,_]](val individuals: ArraySeq[I]) extends Iterable[I] {

  def this(indiv: Iterable[I]) = this(ArraySeq(indiv.toSeq: _*))
  
  override def iterator = individuals.iterator
  
  implicit def individualAndIndexToMultiGoalLike(elt: (I, Int)) = new MultiGoalLike{override def goals = elt._1.goals; def index = elt._2}

  def +=(individual: I)(implicit rng: Random): this.type = {
    val dominatedIndices = dominatesInTheAchive(individual)
    val indivIsDominated = isDominatedInTheAchive(individual)
    
    if(!dominatedIndices.isEmpty && !indivIsDominated) {
      individuals(dominatedIndices(rng.nextInt(dominatedIndices.size))) = individual
    } else { 
      if(!indivIsDominated) {
        val indice = individuals.toIterable.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}.orderByDecreasingCrowding.last._1.index
        individuals(indice) = individual
      }
    }
    this
  }
  
  def ranks(individual: I) = individuals.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}.filter(DominateMinimization.isDominated(individual, _)).map{_.index}.size

  private def dominatesInTheAchive(individual: I) = individuals.zipWithIndex.map{individualAndIndexToMultiGoalLike(_)}.filter(DominateMinimization.isDominated(_, individual)).map{_.index}
  private def isDominatedInTheAchive(individual: I) = individuals.exists(DominateMinimization.isDominated(individual, _))
  
}*/

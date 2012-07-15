/*
 * Copyright (C) 2011 Sebastien Rey
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

package fr.iscpif.mgo.elitism

import annotation.tailrec
import fr.iscpif.mgo.diversity.Diversity
import fr.iscpif.mgo.dominance._
import fr.iscpif.mgo.Individual
import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.ranking.Rank

//Utiliser par NSGA2 et MO-CMA-ES
trait NonDominatedSortingElitism extends Elitism { 
  self: GAEvolution with Archive with Dominance { type MF <: Diversity with Rank } =>

  def elitism(population: Population[G, MF]): Population[G, MF] = {
    
    if (population.size < archiveSize) population
    else {
      val fronts = population.groupBy(_.metaFitness.rank).toList.sortBy(_._1).map { _._2: Population[G, MF] }

      //FIXME: No idea why but it is not tailrec
      def addFronts[I](fronts: List[Population[G, MF]], acc: List[Population[G, MF]], size: Int = 0): (Population[G, MF], Population[G, MF]) = {
        if (size + fronts.head.size < archiveSize) addFronts(fronts.tail, fronts.head :: acc, size + fronts.head.size)
        else (fronts.headOption.getOrElse(Population.empty), acc.flatten)
      }

      val (lastFront, selected) = addFronts(fronts, List.empty)

      
      (if (selected.size < archiveSize) 
        selected ++ lastFront.sortBy(_.metaFitness.diversity).reverse.slice(0, archiveSize - selected.size) 
       else selected)
    }
  }
}

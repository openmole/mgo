/*
 * Copyright (C) 15/07/2015 Guillaume Chérel
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

import scala.collection.immutable.Map
import scala.math._

trait SpeciesRescaledFitnessSharing extends NEATGenome with DoubleFitness with NEATBreeding {

  def speciesOffsprings(
    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
    totalOffsprings: Int): Seq[(Int, Int)] = {

    val speciesAvgFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map {
      case (sp, indivs) => (sp, indivs.map {
        _.fitness
      }.sum / indivs.size)
    }.toSeq

    val lowestSpeciesFitness = speciesAvgFitnesses.minBy { _._2 }._2

    val rescaledSpeciesFitnesses =
      speciesAvgFitnesses.map { case (sp, f) => (sp, f - lowestSpeciesFitness) }

    val sumOfSpeciesFitnesses: Double = rescaledSpeciesFitnesses.map {
      _._2
    }.sum

    if (sumOfSpeciesFitnesses <= 0.0) {
      val numberOfSpecies = indivsBySpecies.size
      indivsBySpecies.keysIterator.map { sp => (sp, max(1, totalOffsprings / numberOfSpecies)) }.toVector
    } else
      rescaledSpeciesFitnesses.map { case (sp, f) => (sp, round(f * totalOffsprings / sumOfSpeciesFitnesses).toInt) }
  }

}

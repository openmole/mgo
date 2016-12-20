///*
// * Copyright (C) 15/07/2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//package mgo.breed
//
//import mgo._
//
//import scala.collection.immutable.Map
//import scala.math._
//
//trait NEATSpeciesRescaledFitnessSharing extends NEATGenome with DoubleFitness with P {
//
//  def speciesOffsprings(
//    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
//    totalOffsprings: Int): Vector[(Int, Int)] = {
//
//    val speciesAvgFitnesses: Vector[(Int, Double)] = indivsBySpecies.map {
//      case (sp, indivs) => (sp, indivs.map {
//        _.fitness
//      }.sum / indivs.size)
//    }.toVector
//
//    val lowestSpeciesFitness = speciesAvgFitnesses.minBy {
//      _._2
//    }._2
//
//    val rescaledSpeciesFitnesses =
//      speciesAvgFitnesses.map { case (sp, f) => (sp, f - lowestSpeciesFitness) }
//
//    val sumOfSpeciesFitnesses: Double = rescaledSpeciesFitnesses.map {
//      _._2
//    }.sum
//
//    /** If the sum of species fitnesses is 0, take an equal number of offsprings for each (fitnesses should never be negative */
//    val result: Vector[(Int, Double)] = if (sumOfSpeciesFitnesses <= 0.0) {
//      val numberOfSpecies = indivsBySpecies.size.toDouble
//      indivsBySpecies.keysIterator.map { sp => (sp, totalOffsprings / numberOfSpecies) }.toVector
//    } else
//      rescaledSpeciesFitnesses.map { case (sp, f) => (sp, f * totalOffsprings / sumOfSpeciesFitnesses) }
//
//    val resultFloored: Vector[(Int, Double, Int)] = result.map { case (sp, nb) => (sp, nb, nb.toInt) }
//
//    /* Rounding errors can result in fewer offsprings than totalOffsprings. To correct the number of offsprings, sort
//  the species by the number of missed offsprings (double value - floored value) and give one to the one that misses the most. */
//    val missingOffsprings = totalOffsprings - result.foldLeft(0) { case (acc, (sp, nb)) => acc + nb.toInt }
//
//    val corrected = (0 until missingOffsprings).foldLeft(resultFloored) {
//      case (acc, _) =>
//        val sortedByNeed: Vector[(Int, Double, Int)] = acc.sortBy { case (sp, nb, nbf) => -(nb - nbf) }
//        val (sp, nb, nbf): (Int, Double, Int) = sortedByNeed(0)
//        sortedByNeed.updated(0, (sp, nb, nbf + 1))
//    }.map { case (sp, nb, nbf) => (sp, nbf) }
//
//    corrected
//  }
//}

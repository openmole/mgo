//package mgo.breed
//
//import mgo._
//
//import scala.collection.immutable.Map
//import scala.math._
//
///**
// * Created by guillaume on 01/07/2015.
// */
//trait NEATSpeciesFitnessSharing extends NEATGenome with DoubleFitness with P {
//
//  def speciesOffsprings(
//    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
//    totalOffsprings: Int): Vector[(Int, Int)] = {
//    val speciesFitnesses: Vector[(Int, Double)] = indivsBySpecies.map {
//      case (sp, indivs) => (sp, indivs.map {
//        _.fitness
//      }.sum / indivs.size)
//    }.toVector
//
//    val sumOfSpeciesFitnesses: Double = speciesFitnesses.map {
//      _._2
//    }.sum
//
//    /** If the sum of species fitnesses is 0, take an equal number of offsprings for each (fitnesses should never be negative */
//    val result: Vector[(Int, Double)] = if (sumOfSpeciesFitnesses <= 0.0) {
//      val numberOfSpecies = indivsBySpecies.size.toDouble
//      indivsBySpecies.keysIterator.map { sp => (sp, totalOffsprings / numberOfSpecies) }.toVector
//    } else
//      speciesFitnesses.map { case (sp, f) => (sp, (f / sumOfSpeciesFitnesses) * totalOffsprings) }
//
//    val resultFloored: Vector[(Int, Double, Int)] = result.map { case (sp, nb) => (sp, nb, nb.toInt) }
//
//    /* Rounding errors can result in fewer offsprings than totalOffsprings. To correct the number of offsprings, sort
//    the species by the number of missed offsprings (double value - floored value) and give one to the one that misses the most. */
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
//
//}

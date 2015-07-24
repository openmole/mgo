package fr.iscpif.mgo.breed

import fr.iscpif.mgo._

import scala.collection.immutable.Map
import scala.math._

/**
 * Created by guillaume on 01/07/2015.
 */
trait SpeciesFitnessSharing extends NEATGenome with DoubleFitness with NEATBreeding {

  def speciesOffsprings(
    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
    totalOffsprings: Int): Seq[(Int, Int)] = {
    val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map {
      case (sp, indivs) => (sp, indivs.map {
        _.fitness
      }.sum / indivs.size)
    }.toSeq
    val sumOfSpeciesFitnesses: Double = speciesFitnesses.map {
      _._2
    }.sum

    if (sumOfSpeciesFitnesses <= 0.0) {
      val numberOfSpecies = indivsBySpecies.size
      indivsBySpecies.keysIterator.map { sp => (sp, max(1, totalOffsprings / numberOfSpecies)) }.toVector
    } else
      speciesFitnesses.map { case (sp, f) => (sp, round(f * totalOffsprings / sumOfSpeciesFitnesses).toInt) }
  }

}

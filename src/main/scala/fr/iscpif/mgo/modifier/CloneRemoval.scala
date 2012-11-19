/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import genome.G

/**
 * Layer of the cake for removing duplicated genomes in the evaluted individuals
 */
trait CloneRemoval extends IndividualFilter with G with F {
  override def filter(individuals: Seq[Individual[G, F]]) =
    individuals.groupBy(_.genome).unzip._2.map { _.head }.toIndexedSeq
}

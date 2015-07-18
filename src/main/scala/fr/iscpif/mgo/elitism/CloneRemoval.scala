/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.G

/**
 * Layer of the cake for removing duplicated genomes in the evaluated individuals
 */
trait CloneRemoval extends IndividualFilter with G with P with F {

  override def filters =
    super.filters ++ Seq(
      (population: Population[G, P, F]) =>
        population.groupBy(i => genomesEqualOn(i.genome)).values.map { _.sortBy(_.age).head }.toIndexedSeq: Population[G, P, F]
    )
}

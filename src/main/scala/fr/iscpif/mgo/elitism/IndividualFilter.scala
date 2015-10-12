/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.elitism

import fr.iscpif.mgo._

import scalaz._

/**
 * Layer of the cake for filtering individuals in the set of evaluated individuals
 */
trait IndividualFilter extends G with F with P {

  type FILTER = (Population[G, P, F]) => Population[G, P, F]
  def filters = Seq.empty[FILTER]

  /**
   * Filter the individuals
   *
   * @param population the set of evaluated individuals
   * @return the filtrated individuals
   */
  def filter(population: Population[G, P, F]): Population[G, P, F] = filters.foldLeft(population)((p, f) => f(p))
}

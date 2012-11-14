/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import genome.G

/**
 * Layer of the cake for filtering individuals in the set of evaluted individuals
 */
trait IndividualFilter extends G with F {
  
  /**
   * Filter the individuals
   * 
   * @param individuals the set of evaluated individuals
   * @return the filtrated individuals
   */
  def filter(individuals: IndexedSeq[Individual[G, F]]) = individuals
}

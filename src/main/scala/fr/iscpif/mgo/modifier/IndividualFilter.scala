/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

/**
 * Layer of the cake for filtering individuals in the set of evaluted individuals
 */
trait IndividualFilter extends G {
  
  /**
   * Filter the individuals
   * 
   * @param individuals the set of evaluated individuals
   * @return the filtrated individuals
   */
  def filter(individuals: IndexedSeq[Individual[G]]) = individuals
}

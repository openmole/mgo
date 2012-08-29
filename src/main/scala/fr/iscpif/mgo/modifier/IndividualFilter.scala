/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

trait IndividualFilter extends G {
  def filter(individuals: IndexedSeq[Individual[G]]) = individuals
}

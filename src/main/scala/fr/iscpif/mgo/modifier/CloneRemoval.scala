/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

trait CloneRemoval extends IndividualFilter with G {
  override def filter(individuals: IndexedSeq[Individual[G]]) = 
    individuals.groupBy(_.genome).unzip._2.map{_.head}.toIndexedSeq
}

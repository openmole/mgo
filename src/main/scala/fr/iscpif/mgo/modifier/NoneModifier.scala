/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

trait NoneModifier extends Modifier { this: Evolution =>
  
  type MF = None.type
  
  def toPopulation(e: IndexedSeq[Individual[G]]) =
    new Population[G, MF] {
      lazy val content = e.map{ PopulationElement(_, None) }
    }
  
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

trait Modifier { this: Evolution =>
  def toPopulation(individuals: IndexedSeq[(G, Fitness)]): Population[G, I]
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._

trait NoneModifier extends Modifier { this: Evolution =>
  
  type I = Individual[G]
  
  def toPopulation(e: IndexedSeq[(G, Fitness)]) =
    new Population[G, I] {
      lazy val content = e.map{case(g, f) => PopulationElement(g, f, Individual(g, f))}
    }
  
}

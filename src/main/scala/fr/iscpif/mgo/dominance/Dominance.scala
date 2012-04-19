/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.dominance

import fr.iscpif.mgo._

trait Dominance { this: Evolution =>
  
  def isDominated (p1: FIT, p2: FIT): Boolean
  
  /*implicit def indiviudal2Dominant[F <: GAFitness](i: Individual[_,F]) = new {
    def isDominated(i2: Individual[_,F]) = dominance.isDominated(i, i2)
  }
  
  implicit def gafitness2Dominant(f: GAFitness) = new {
    def isDominated(f2: GAFitness) = dominance.isDominated(f, f2)
  }*/
}

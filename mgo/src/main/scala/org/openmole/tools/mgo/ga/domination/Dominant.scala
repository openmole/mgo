/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.domination


import org.openmole.tools.mgo.ga.GAFitness
import org.openmole.tools.mgo.Individual._
import org.openmole.tools.mgo.Individual

trait Dominant {
  dominant =>
  
  def isDominated (p1: GAFitness, p2: GAFitness): Boolean
  
  implicit def indiviudal2Dominant[F <: GAFitness](i: Individual[_,F]) = new {
    def isDominated(i2: Individual[_,F]) = dominant.isDominated(i, i2)
  }
  
  implicit def gafitness2Dominant(f: GAFitness) = new {
    def isDominated(f2: GAFitness) = dominant.isDominated(f, f2)
  }
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators.crossover

import fr.iscpif.mgo._
import ga._
import tools.Random._
import java.util.Random

class AverageCrossover [G <: GAGenome, F <: GAGenomeFactory [G]] extends CrossOver [G, F] {
  def apply (g1: G, g2: G, factory: F) (implicit aprng : Random) = {
    val pds = aprng.nextDouble
      
    val newValues = IndexedSeq.tabulate (g1.values.size) (i => 
      (pds*g1.values (i) + (1 - pds) * g2.values (i)) / 2)
    IndexedSeq(factory(newValues))
  }
}


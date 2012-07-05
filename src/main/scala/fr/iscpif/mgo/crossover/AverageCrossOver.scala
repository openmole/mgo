/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import java.util.Random

trait AverageCrossover extends CrossOver { self: GAEvolution =>
  
  def crossover (g1: G, g2: G) (implicit aprng : Random, factory: Factory[G]) = {
    val pds = aprng.nextDouble
      
    val newValues = IndexedSeq.tabulate (g1.values.size) (i => 
      (pds*g1.values (i) + (1 - pds) * g2.values (i)) / 2)

        
    
    IndexedSeq(factory(newValues))
  }
}


/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo._
import ga._
import tools.Random._
import java.util.Random

class AverageMutation [G <: GAGenome, F <: GAGenomeFactory [G]] (val factory : F) 
  extends Mutation [G, F] {
  override def operate (genomes : IndexedSeq [G]) (implicit aprng : Random) : G = {
    val g1 = genomes.random
    val g2 = genomes.random
      
    val pds = aprng.nextDouble
      
    val newValues = IndexedSeq.tabulate (g1.values.size) (i => 
      (pds*g1.values (i) + (1 - pds) * g2.values (i)) / 2)
    factory.buildGenome (newValues)
  }
}


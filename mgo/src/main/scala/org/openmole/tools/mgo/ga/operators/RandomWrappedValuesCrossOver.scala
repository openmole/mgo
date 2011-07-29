/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo._
import ga._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._

class RandomWrappedValuesCrossOver [G <: GAGenome, F <: GAGenomeFactory [G]] (implicit val factory : F)
  extends CrossOver [G, F] {
  def operate (genomes : IndexedSeq [G]) (implicit aprng : Random) : G = {
    val g1 = genomes.random
    val g2 = genomes.random
    
    factory.buildGenome (
      IndexedSeq.tabulate (g1.wrappedValues.size) (i => 
        if (aprng.nextBoolean) g1.wrappedValues (i) else g2.wrappedValues (i))
    )
  }
}


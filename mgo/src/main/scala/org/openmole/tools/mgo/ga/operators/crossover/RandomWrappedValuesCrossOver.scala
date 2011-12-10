/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo._
import ga._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._

// TODO: Self-Adaptive pour crossover; à chercher ?
class RandomWrappedValuesCrossOver [G <: GAGenome, F <: GAGenomeFactory [G]] (rate: Random => Double = rng => rng.nextFloat)(implicit val factory : F)
  extends CrossOver [G, F] {
    
  def this( rate: Double, factory : F) = this( _ => rate)(factory)
  
  def operate (genomes : IndexedSeq [G]) (implicit aprng : Random) : IndexedSeq[G] = {
    val g1 = genomes.random
    val g2 = genomes.random
    val crossoverRate = rate(aprng)
    
    // False on echange, true on maintient
    val rngValue:IndexedSeq[Boolean] = (0 until g1.wrappedValues.size).map{x => if (aprng.nextDouble < crossoverRate) false else true }
    val offspringValues = (g1.wrappedValues,g2.wrappedValues,rngValue).zipped.map((x, y, z) => if (z) IndexedSeq(x, y) else IndexedSeq(y, x)).transpose
    
    return offspringValues.map{factory.buildGenome (_)}.toIndexedSeq

  }

}


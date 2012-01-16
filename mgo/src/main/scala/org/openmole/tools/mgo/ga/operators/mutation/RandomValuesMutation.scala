/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import org.openmole.tools.mgo.ga._

class RandomValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G]] (
  rate: Random => Double = rng => rng.nextFloat) 
  extends Mutation [G, F] {
  
  def this (rate : Double) = this( _ => rate)
  
  override def mutate(genomes: IndexedSeq[G], factory: F)(implicit aprng: Random): G = {
   
    val mutationRate = rate(aprng)
    val genome = genomes.random
    /* @todo Faire en sorte d'utiliser plutot un genome généré par buildRandomGenome, 
     * plutot qu'une valeur tiré au hasard avec aprng... */
    val newValues = genome.values map { v => 
      if (aprng.nextDouble < mutationRate) aprng.nextDouble
      else v 
    }
    return  factory.buildFromValues (genome,newValues)
  }
  
}

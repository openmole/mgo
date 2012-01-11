/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import org.openmole.tools.mgo.ga._


class RandomWrappedValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G]] (
  rate: Random => Double = rng => rng.nextFloat) (implicit val factory : F) 
  extends Mutation [G, F]  {
  
  def this( rate: Double, factory : F) = this( _ => rate)(factory)
  
  override def operate(genomes: IndexedSeq[G])(implicit aprng: Random): G = {
    
    val mutationRate = rate(aprng)
    println("mutation rate = " + mutationRate)
    val genome = genomes.random
    
    val randomGenome = factory.buildRandomGenome
    val valMutationZipped = genome.wrappedValues.zip(randomGenome.wrappedValues)   
    val newValues = valMutationZipped map { case(v,vrg) => 
      if (aprng.nextDouble < mutationRate) 
        vrg
      else v }
    
    return factory.buildGenome (newValues)
  }
  
}

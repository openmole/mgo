/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators.mutation

import org.openmole.tools.mgo._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import org.openmole.tools.mgo.ga._


class RandomWrappedValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G]] (
  rate: Random => Double = rng => rng.nextFloat) 
  extends Mutation [G, F]  {
  
  def this( rate: Double) = this( _ => rate)
  
  override def mutate(genomes: IndexedSeq[G], factory: F)(implicit aprng: Random): G = {
    val mutationRate = rate(aprng)
    val genome = genomes.random
    
    val randomGenome = factory.random
    val valMutationZipped = genome.wrappedValues.zip(randomGenome.wrappedValues)   
    val newValues = valMutationZipped map { 
      case(v,vrg) => 
        if (aprng.nextDouble < mutationRate) vrg else v 
    }
    
    factory(newValues)
  }
  
}

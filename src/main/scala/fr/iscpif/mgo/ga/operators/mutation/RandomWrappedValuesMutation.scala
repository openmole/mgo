/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators.mutation

import fr.iscpif.mgo._
import java.util.Random
import fr.iscpif.mgo.tools.Random._
import fr.iscpif.mgo.ga._


class RandomWrappedValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G]] (
  rate: Random => Double = rng => rng.nextFloat) 
  extends Mutation [G, F]  {
  
  def this( rate: Double) = this( _ => rate)
  
  override def apply(genome: G, factory: F)(implicit aprng: Random): G = {
    val mutationRate = rate(aprng)
    
    val randomGenome = factory.random
    val valMutationZipped = genome.wrappedValues.zip(randomGenome.wrappedValues)   
    val newValues = valMutationZipped map { 
      case(v,vrg) => 
        if (aprng.nextDouble < mutationRate) vrg else v 
    }
    
    factory(newValues)
  }
  
}

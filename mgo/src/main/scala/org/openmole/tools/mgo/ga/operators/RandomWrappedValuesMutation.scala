/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import org.openmole.tools.mgo.genomefactory._

class RandomWrappedValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G] with FromWrappedValuesFactory [G]](implicit val factory : F)  //(implicit rate: Random => Double, val factory : F) 
  extends Mutation [G, F]  {
  
  //def this( rate: Double, factory : F) = this( _ => rate, factory)
  
  override def operate(genomes: IndexedSeq[G])(implicit aprng: Random): G = {
   
    /* @todo Faire marcher le mutation rate .... pour qu'il puisse etre passé en parametre */
    
    //val mutationRate = rate(aprng)
    val mutationRate = 0.6
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

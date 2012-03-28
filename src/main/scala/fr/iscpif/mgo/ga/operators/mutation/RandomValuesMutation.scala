/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators.mutation

import fr.iscpif.mgo._
import java.util.Random
import fr.iscpif.mgo.tools.Random._
import fr.iscpif.mgo.ga._

class RandomValuesMutation [
  G <: GAGenome, 
  F <: GAGenomeFactory [G]] (
  rate: Random => Double = rng => rng.nextFloat) 
  extends Mutation [G, F] {
  
  def this (rate : Double) = this( _ => rate)
  
  override def apply(genome: G, factory: F)(implicit aprng: Random): G = {
   
    val mutationRate = rate(aprng)
    /* @todo Faire en sorte d'utiliser plutot un genome généré par buildRandomGenome, 
     * plutot qu'une valeur tiré au hasard avec aprng... */
    val newValues = genome.values map { v => 
      if (aprng.nextDouble < mutationRate) aprng.nextDouble
      else v 
    }
    return  factory.updatedValues (genome, newValues)
  }
  
}

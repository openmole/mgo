/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openmole.tools.mgo.ga.operators.mutation

import org.openmole.tools.mgo._
import ga._
import tools.Random._
import tools.Math._
import java.util.Random

class FixedGaussianMutation [G <: GAGenome, F <: GAGenomeFactory [G]] (
  sigma : Double) 
  extends Mutation [G, F] {

  override def mutate (genomes:IndexedSeq[G], factory: F) (implicit aprng: Random) : G = {
    val genome = genomes.random
    val newValues = genome.values map (v => 
      clamp (v + (aprng.nextGaussian * sigma), 0, 1))
    return factory.buildGenome (newValues)
  }  
}
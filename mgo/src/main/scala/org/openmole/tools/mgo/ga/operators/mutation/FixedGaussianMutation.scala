/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo._
import ga._
import tools.Random._
import tools.Math._
import java.util.Random

class FixedGaussianMutation [G <: GAGenome, F <: GAGenomeFactory [G]] (
  sigma : Double) (implicit val factory : F)
  extends Mutation [G, F] {

  override def operate (genomes:IndexedSeq[G]) (implicit aprng: Random) : G = {
    val genome = genomes.random
    val newValues = genome.values map (v => 
      clamp (v + (aprng.nextGaussian * sigma), 0, 1))
    return factory.buildGenome (newValues)
  }  
}
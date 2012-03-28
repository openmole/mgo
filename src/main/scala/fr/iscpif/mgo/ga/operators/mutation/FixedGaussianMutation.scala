/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.iscpif.mgo.ga.operators.mutation

import fr.iscpif.mgo._
import ga._
import tools.Random._
import tools.Math._
import java.util.Random

class FixedGaussianMutation [G <: GAGenome, F <: GAGenomeFactory [G]] (
  sigma : Double) 
  extends Mutation [G, F] {

  override def apply (genome: G, factory: F) (implicit aprng: Random) : G = {
    val newValues = genome.values map (v => 
      clamp (v + (aprng.nextGaussian * sigma), 0, 1))
    factory(newValues)
  }  
}
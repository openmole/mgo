/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import tools.Random._
import tools.Math._
import java.util.Random

trait FixedGaussianMutation extends Mutation {
  self: GAEvolution =>
   
  def sigma : Double

  override def mutate (genome: G, factory: F) (implicit aprng: Random) : G = {
    val newValues = genome.values map (v => 
      clamp (v + (aprng.nextGaussian * sigma), 0, 1))
    factory(newValues)
  }  
}
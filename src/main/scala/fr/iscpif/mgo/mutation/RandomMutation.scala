/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.mutation

import java.util.Random
import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.tools.Random._

trait RandomMutation extends Mutation  { 
  self: GAEvolution =>
    
  def mutationRate = 0.5

  override def mutate(genome: G)(implicit aprng: Random, factory: Factory[G]): G = {
    val randomGenome = factory.random
    val valMutationZipped = genome.content.zip(randomGenome.content)   
    val newValues = valMutationZipped map { 
      case(v,vrg) => 
        if (aprng.nextDouble < mutationRate) vrg else v 
    }
    
    factory(newValues)
  }
  
}

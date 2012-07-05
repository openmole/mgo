/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.mutation

import java.util.Random
import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.tools.Random._

trait RandomValuesMutation extends Mutation { 
  self: GAEvolution =>
  
  def mutationRate: Double = 0.5
  
  override def mutate(genome: G)(implicit aprng: Random, factory: Factory[G]): G = {
   
    /* FIXME Faire en sorte d'utiliser plutot un genome généré par buildRandomGenome, 
     * plutot qu'une valeur tiré au hasard avec aprng... */
    val newValues = genome.values map { v => 
      if (aprng.nextDouble < mutationRate) aprng.nextDouble
      else v 
    }
    factory(genome.updatedValues(newValues))
  }
  
}

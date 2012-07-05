/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import tools.Math._
import tools.Random._
import java.util.Random

trait DynamicGaussianMutation extends Mutation  { 
  self: SigmaGAEvolution =>
  
  //http://c-faq.com/lib/gaussian.html
  //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
  //http://www.taygeta.com/random/gaussian.html
  override def mutate (genome : G) (implicit aprng : Random, factory: Factory[G]) : G = {
    val newValues = (genome.values zip genome.sigma) map {
      case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)
    }
    factory(genome.updatedValues(newValues))
  }
}

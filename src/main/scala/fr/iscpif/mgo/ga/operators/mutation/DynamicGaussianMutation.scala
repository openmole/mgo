/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators.mutation

import fr.iscpif.mgo._
import ga._
import tools.Math._
import tools.Random._
import java.util.Random

class DynamicGaussianMutation [G <: GAGenome with SigmaParameters, 
                               F <: GAGenomeFactory[G] with GASigmaParametersFactory [G]] 
  extends Mutation [G, F] {

  //http://c-faq.com/lib/gaussian.html
  //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
  //http://www.taygeta.com/random/gaussian.html
  override def apply (genome : G, factory: F) (implicit aprng : Random) : G = {
    val newValues = (genome.values zip genome.sigma) map {
      case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)
    }
    factory.updatedValues (genome, newValues)
  }
}

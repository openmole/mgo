/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo._
import ga._
import tools.Math._
import tools.Random._
import java.util.Random

class EvolvingSoftGaussianMutation [G <: GAGenome with SigmaParameters, 
                                    F <: GAGenomeFactory [G]] (
  implicit val factory : F) extends Mutation [G, F] {

  //http://c-faq.com/lib/gaussian.html
  //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
  override def operate (genomes : IndexedSeq [G]) (implicit aprng : Random) : G = {
    val genome = genomes.random 
    val newValues = (genome.values zip genome.sigma) map {
      case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)}
    factory.buildFromValues (genome, newValues)
    
  }
}

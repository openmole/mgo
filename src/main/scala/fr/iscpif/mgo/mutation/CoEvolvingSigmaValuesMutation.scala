package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import tools.Math._
import tools.Random._
import java.util.Random
import scala.math._


trait CoEvolvingSigmaValuesMutation extends Mutation  { 
  self: GAEvolution {type G <: GAGenome with Sigma} =>
  
  override def mutate (genome : G) (implicit aprng : Random, factory: Factory[G]) : G = {
    // See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
    // See on paper : Gaussian mutation and self adaptation (Hinterding) && Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It, Modern Heuristics (assez détaillé !)
    val indexedSeqSigma = genome.sigma.map{ s => clamp(s * exp(aprng.nextGaussian),0 ,1) }
    
    val newValues = 
      (genome.values zip indexedSeqSigma) map {
        case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)
      }
    factory(genome, _.updatedValues(newValues), _.updatedSigma(indexedSeqSigma))
  }
}
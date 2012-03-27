package fr.iscpif.mgo.ga.operators.mutation

import fr.iscpif.mgo._
import ga._
import fr.iscpif.mgo.ga._
import tools.Math._
import tools.Random._
import java.util.Random
import scala.math._

class CoEvolvingSigmaValuesMutation 
  [G <: GAGenome with SigmaParameters, 
   F <: GAGenomeFactory[G] with GASigmaParametersFactory[G]]
   extends Mutation [G, F] {
  
  override def mutate (genomes : IndexedSeq [G], factory: F) (implicit aprng : Random) : G = {
    val genome = genomes.random
    
    // See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
    // See on paper : Gaussian mutation and self adaptation (Hinterding) && Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It, Modern Heuristics (assez détaillé !)
    val indexedSeqSigma = genome.sigma.map{ s => clamp(s * exp(aprng.nextGaussian),0 ,1) }
    
    val newValues = 
      (genome.values zip indexedSeqSigma) map {
        case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)
      }
    
    factory.updatedValuesSigma(genome, newValues, indexedSeqSigma)
  }
}
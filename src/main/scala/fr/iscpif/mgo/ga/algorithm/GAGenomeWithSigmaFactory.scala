/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import java.util.Random
import fr.iscpif.mgo.ga.GAGenomeFactory
import fr.iscpif.mgo.ga.GASigmaParametersFactory

class GAGenomeWithSigmaFactory(genomeSize: Int) extends GAGenomeFactory[GAGenomeWithSigma] with GASigmaParametersFactory [GAGenomeWithSigma] {

  override def apply(v : IndexedSeq[Double]) =
    new GAGenomeWithSigma(genomeSize) {
      val wrappedValues = v
    }
    
  override def updatedValues(genome: GAGenomeWithSigma, _values: IndexedSeq [Double]) = 
    new GAGenomeWithSigma(genomeSize) {
      val wrappedValues = _values ++ genome.sigma
    }

  override def updatedSigma(genome: GAGenomeWithSigma, _sigma: IndexedSeq [Double]) = 
    new GAGenomeWithSigma(genomeSize) {
      val wrappedValues = genome.values ++ _sigma
    }
      
  def random (implicit aprng : Random) = 
    new GAGenomeWithSigma(genomeSize){
      val wrappedValues = (0 until size * 2).map{_ => aprng.nextDouble}
    }

}
  

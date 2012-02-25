/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.algorithm

import java.util.Random
import org.openmole.tools.mgo.ga.GAGenomeFactory
import org.openmole.tools.mgo.ga.GASigmaParametersFactory

class GAGenomeWithSigmaFactory(size: Int) extends GAGenomeFactory[GAGenomeWithSigma] with GASigmaParametersFactory [GAGenomeWithSigma] {
  override def buildGenome(v : IndexedSeq[Double]) = 
    new GAGenomeWithSigma(size) {
      val wrappedValues = v
    }
    
  override def buildFromValues(genome: GAGenomeWithSigma, _values: IndexedSeq [Double]) = 
    new GAGenomeWithSigma(size) {
      val wrappedValues = _values ++ genome.sigma
    }

  override def buildFromSigma(genome: GAGenomeWithSigma, _sigma: IndexedSeq [Double]) = 
    new GAGenomeWithSigma(size) {
      val wrappedValues = genome.values ++ _sigma
    }
      
  def buildRandomGenome (implicit aprng : Random) = 
    new GAGenomeWithSigma(size){ 
      val wrappedValues = (0 until size * 2).map{_ => aprng.nextDouble}
    }

}
  

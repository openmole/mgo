/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo.AbstractGenome
import org.openmole.tools.mgo.GenomeFactory
import org.openmole.tools.mgo.Mutation
import org.openmole.tools.mgo.ga.SigmaParameters
import org.openmole.tools.mgo.tools.Random._
import java.util.Random

class FixedSoftGaussianMutation [G <: AbstractGenome with SigmaParameters, 
                                 F <: GenomeFactory [G]] (val factory:F) 
  extends Mutation [G, F] {

  def operate (genomes:IndexedSeq[G])
    (implicit aprng: Random) : Double = {
      
    val pickedGenome = genomes.random
    
    val gRnd = aprng.nextGaussian()
    val gRndAffine =  ( gRnd  * computeSigma (min, max, 6.0)) + value
    return 0 //clamp(gRndAffine, min, max)
  }
  
  def computeSigma (min : Double, max : Double, precision : Double) : Double = {
    return  (max - min) / precision
  }
  
}
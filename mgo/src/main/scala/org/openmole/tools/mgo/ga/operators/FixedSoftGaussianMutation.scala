/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators

import org.openmole.tools.mgo.AbstractGenome
import org.openmole.tools.mgo.GenomeFactory
import org.openmole.tools.mgo.Mutation
import org.openmole.tools.mgo.ga.SigmaParameters
import java.util.Random


class FixedSoftGaussianMutation [G <: AbstractGenome with SigmaParameters, 
                                 F <: GenomeFactory [G]] 
  extends Mutation [G, F] {

  def operateMutation (value : Double, min : Double, max : Double)
    (implicit rng: Random) : Double = {
    val gRnd = rng.nextGaussian()
    val gRndAffine =  ( gRnd  * computeSigma (min, max, 6.0)) + value
    return 0 //clamp(gRndAffine, min, max)
  }
  
  def computeSigma (min : Double, max : Double, precision : Double) : Double = {
    return  (max - min) / precision
  }
  
}
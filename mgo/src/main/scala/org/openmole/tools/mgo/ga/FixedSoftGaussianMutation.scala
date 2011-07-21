/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

import org.openmole.tools.mgo.mappedgenome.genomedouble._
import org.apache.commons.math.util.FastMath._
import org.openmole.tools.mgo.tools.Random._
import java.util.Random

class FixedSoftGaussianMutation (interval: IntervalSet,rate: Random => Double = rng => rng.nextFloat) extends GenericMutation(interval,rate) {

   def operateMutation(value:Double,min:Double,max:Double)(implicit rng: Random):Double= {
    val gRnd = rng.nextGaussian()
    val gRndAffine =  ( gRnd  * computeSigma(min,max,6.0)) + value
    return clamp(gRndAffine,min,max)
  }
  
  def computeSigma(min:Double,max:Double,precision:Double):Double={
    return  (max - min) / precision
  }
  
}

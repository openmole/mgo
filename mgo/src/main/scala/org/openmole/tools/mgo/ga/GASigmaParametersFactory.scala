/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo._

trait GASigmaParametersFactory [G <: GAGenome with SigmaParameters] { this: GAGenomeFactory[G] =>
  def updatedSigma(genome: G, sigma: IndexedSeq [Double]): G
  def updatedValuesSigma(genome: G, values: IndexedSeq [Double], sigma: IndexedSeq[Double]) =
    updatedValues(updatedSigma(genome, sigma), values)
}
 

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo._

trait GASigmaParametersFactory [G <: GAGenome with SigmaParameters] { this: GAGenomeFactory[G] =>
  def buildFromSigma(genome: G, sigma: IndexedSeq [Double]): G
  def buildFromSigmaAndValues(genome: G, values: IndexedSeq [Double], sigma: IndexedSeq[Double]) =
    buildFromValues(
      buildFromSigma(genome, sigma),
      values)
}
 

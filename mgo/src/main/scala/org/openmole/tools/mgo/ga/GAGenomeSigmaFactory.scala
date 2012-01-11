/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo._

trait GAGenomeSigmaFactory [G <: GAGenome with SigmaParameters] extends GAGenomeFactory [G] {
   def buildFromSigma(genome: G, sigma: IndexedSeq [Double]): G
}
 

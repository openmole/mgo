/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo.ga._
import fr.iscpif.mgo._

trait GASigmaParametersFactory [G <: GAGenome with SigmaParameters] { this: GAGenomeFactory[G] =>
  def updatedSigma(genome: G, sigma: IndexedSeq [Double]): G
  def updatedValuesSigma(genome: G, values: IndexedSeq [Double], sigma: IndexedSeq[Double]) =
    updatedValues(updatedSigma(genome, sigma), values)
}
 

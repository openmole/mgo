/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.genomefactory

import org.openmole.tools.mgo.ga.GAGenome

trait FromSigmaValuesFactory[G <: GAGenome] {
  def buildFromSigmaValues(genome: G, values: IndexedSeq [Double]): G 
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.genomefactory

import org.openmole.tools.mgo.ga.GAGenome

trait FromValuesFactory[G <: GAGenome] {
  def buildFromValues(genome: G, values: IndexedSeq [Double]): G 
}

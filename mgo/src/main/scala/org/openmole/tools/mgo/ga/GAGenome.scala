/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.Genome

trait GAGenome extends Genome {
  val wrappedValues = values
  val values : IndexedSeq [Double] 
}

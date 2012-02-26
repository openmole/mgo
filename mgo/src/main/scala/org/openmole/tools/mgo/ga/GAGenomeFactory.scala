/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo._
import org.openmole.tools.mgo.ga._
import java.util.Random

trait GAGenomeFactory [G <: GAGenome] {
  def apply (v: IndexedSeq [Double]) : G 
  def updatedValues(g: G, values: IndexedSeq [Double]): G
  def random (implicit aprng: Random): G
}

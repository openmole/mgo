/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

trait FromValuesFactory[G] {
  def buildFromValues(genome: G, values: Array[Double]): G 
}

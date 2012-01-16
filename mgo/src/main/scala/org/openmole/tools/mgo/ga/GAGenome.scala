/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga


trait GAGenome {
  def wrappedValues : IndexedSeq [Double]
  def values : IndexedSeq [Double] 
  
  override def toString = wrappedValues.toString
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo.Genome

trait GAGenome extends Genome {
  def wrappedValues : IndexedSeq [Double]
  def values : IndexedSeq [Double] 
  
  def updatedValues(values: IndexedSeq [Double]): GAGenome
  
  override def toString = wrappedValues.toString
}

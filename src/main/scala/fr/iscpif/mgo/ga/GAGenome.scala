/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo._

trait GAGenome extends Genome {
  type T = IndexedSeq[Double]
  
  def content: T
  
  def values: IndexedSeq[Double] 
  def updatedValues(values: IndexedSeq[Double]): T
  
  override def toString = content.toString
}

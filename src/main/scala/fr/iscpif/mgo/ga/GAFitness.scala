/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

object GAFitness {

  implicit def indexedSeqToFit(f: IndexedSeq[Double]) = new {
    def toGAFitness = new GAFitness {
      val values = f
    }
  }
  
}


trait GAFitness {
  def values: IndexedSeq[Double]
  override def toString = values.toString
}

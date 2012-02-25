/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

object GAFitness {

  implicit def indexedSeqToFit(f: IndexedSeq[Double]) = new {
    def toGAFitness = new GAFitness {
      val fitness = f
    }
  }
  
}


trait GAFitness {
  def fitness: IndexedSeq[Double]
  override def toString = fitness.toString
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

object Fitness {

  implicit def indexedSeqToFit(f: IndexedSeq[Double]) = new {
    def toGAFitness = new Fitness {
      val values = f
    }
  }
  
  def apply(v: Traversable[Double]) = new Fitness {
    val values = v.toIndexedSeq
  }
  
}


trait Fitness {
  def values: IndexedSeq[Double]
  override def toString = values.toString
}

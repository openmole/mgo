/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

trait Sigma { self: GAGenome =>
  def sigma: IndexedSeq[Double]
  
  def updatedSigma(sigma: IndexedSeq [Double]): IndexedSeq[Double]
}

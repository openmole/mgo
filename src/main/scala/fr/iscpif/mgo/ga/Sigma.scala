/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

trait Sigma { self: GAGenome =>
  def sigma: IndexedSeq[Double]
  
  def updatedValues(values: IndexedSeq [Double]): GAGenome with Sigma
  def updatedSigma(sigma: IndexedSeq [Double]): GAGenome with Sigma
  def updatedValuesSigma(values: IndexedSeq [Double], sigma: IndexedSeq[Double]) = updatedSigma(sigma).updatedValues(values)

}

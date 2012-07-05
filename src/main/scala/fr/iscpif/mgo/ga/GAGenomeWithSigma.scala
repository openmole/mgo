/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo.Factory
import java.util.Random

object GAGenomeWithSigma {
  def factory(size: Int) = 
    new Factory[GAGenomeWithSigma] {
      def apply(content: IndexedSeq[Double]) = {
        assert(content.size / 2 == size)
        new GAGenomeWithSigma(
          content.slice(0, content.size / 2),
          content.slice(content.size / 2, content.size)
        )
      }
      
      def random(implicit rng: Random) = apply(Stream.continually(rng.nextDouble).take(size * 2).toIndexedSeq)
    }
}



case class GAGenomeWithSigma(
  val values: IndexedSeq[Double],
  val sigma: IndexedSeq[Double]) extends GAGenome with Sigma {
  
  def content = values ++ sigma
  
  override def updatedValues(values: IndexedSeq [Double]) = copy(values = values).content

  override def updatedSigma(sigma: IndexedSeq [Double]) = copy(sigma = sigma).content
                                  
}




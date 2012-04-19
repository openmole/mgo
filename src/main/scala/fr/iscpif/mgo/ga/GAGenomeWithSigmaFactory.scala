/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import java.util.Random
import fr.iscpif.mgo._

trait GAGenomeWithSigmaFactory extends GAGenomeFactory[GAGenomeWithSigma] {
  factory =>

  def size: Int
    
  def apply(v: IndexedSeq[Double]) = 
    new GAGenomeWithSigma {
      def values = v.slice(0, factory.size)
      def sigma = v.slice(factory.size, factory.size *2)
      def size = factory.size
    }
  
  def random (implicit aprng : Random) = 
    new GAGenomeWithSigma {
      def values = (0 until size).map{_ => aprng.nextDouble}
      def sigma = (0 until size).map{_ => aprng.nextDouble}
      def size = factory.size
    }
    
}
  

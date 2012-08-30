/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo._
import java.util.Random

/**
 * Factory associated with genomes with sigma
 */
trait GAGenomeWithSigmaFactory extends Factory[GAGenomeWithSigma] {
  
  /// Size of the value part of the genome
  def size: Int
  
  def apply(content: GAGenomeWithSigma#T) = {
    assert(content.size / 2 == size)
    new GAGenomeWithSigma(
      content.slice(0, content.size / 2),
      content.slice(content.size / 2, content.size)
    )
  }
      
  def random(implicit rng: Random) = apply(Stream.continually(rng.nextDouble).take(size * 2).toIndexedSeq)
    
}

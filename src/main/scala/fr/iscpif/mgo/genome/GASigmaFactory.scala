/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.genome

import fr.iscpif.mgo._
import util.Random

/**
 * Factory associated with genomes with sigma
 */
trait GASigmaFactory extends GenomeFactory with GASigma {

  /** Size of the value part of the genome */
  def genomeSize: Int

  def genomeFactory: Factory[G] = new Factory[G] {
    override def apply(content: GAGenomeWithSigma#T) = {
      assert(content.size / 2 == genomeSize)
      GAGenomeWithSigma(
        content.slice(0, content.size / 2),
        content.slice(content.size / 2, content.size)
      )
    }

    override def random(implicit rng: Random) = apply(Stream.continually(rng.nextDouble).take(genomeSize * 2).toIndexedSeq)
  }
}

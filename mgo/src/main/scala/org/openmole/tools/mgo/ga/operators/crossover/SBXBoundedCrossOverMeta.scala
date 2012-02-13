/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators.crossover

import java.util.Random
import org.openmole.tools.mgo.ga.GAGenome
import org.openmole.tools.mgo.ga.GAGenomeFactory
import org.openmole.tools.mgo.ga.SBXParameters
import org.openmole.tools.mgo.tools.Random._

class SBXBoundedCrossoverMeta[G <: GAGenome with SBXParameters, F <: GAGenomeFactory[G]](
  distributionIndexScale: Double) extends AbstractSBXBoundedCrossover[G, F] {

  def crossOver (genomes : IndexedSeq [G], factory: F) (implicit aprng : Random) = {
    val g1 = genomes.random
    val g2 = genomes.random
    val distributionIndex = if(aprng.nextBoolean) g1.distributionIndex else g2.distributionIndex
    val rate = if(aprng.nextBoolean) g1.crossoverRate else g2.crossoverRate
    crossOver(g1, g2, distributionIndex * distributionIndexScale, rate, factory)
  }
  
}

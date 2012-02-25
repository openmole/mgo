/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.algorithm

import org.openmole.tools.mgo.ga.operators.crossover.SBXBoundedCrossover
import org.openmole.tools.mgo.ga.operators.mutation.CoEvolvingSigmaValuesMutation

class SigmaNSGAII(sbxDistributionIndex: Double) extends NSGAII(
  new CoEvolvingSigmaValuesMutation[GAGenomeWithSigma, GAGenomeWithSigmaFactory],
  new SBXBoundedCrossover[GAGenomeWithSigma, GAGenomeWithSigmaFactory](sbxDistributionIndex))

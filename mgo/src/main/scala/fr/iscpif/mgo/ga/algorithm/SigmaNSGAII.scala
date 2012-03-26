/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm

import fr.iscpif.mgo.ga.operators.crossover.SBXBoundedCrossover
import fr.iscpif.mgo.ga.operators.mutation.CoEvolvingSigmaValuesMutation

class SigmaNSGAII(sbxDistributionIndex: Double) extends NSGAII(
  new CoEvolvingSigmaValuesMutation[GAGenomeWithSigma, GAGenomeWithSigmaFactory],
  new SBXBoundedCrossover[GAGenomeWithSigma, GAGenomeWithSigmaFactory](sbxDistributionIndex))

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg

import org.openmole.tools.mgo.Genome
import org.openmole.tools.mgo.model.MultiGoalLike

trait IndividualMGFactory[MG <: MultiGoalLike,G <: Genome,I <: IndividualMG[G,MG]] {
  def operate(genome:G):I
}

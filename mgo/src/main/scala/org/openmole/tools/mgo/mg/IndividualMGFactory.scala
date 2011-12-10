/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mg

import org.openmole.tools.mgo.AbstractGenome
import org.openmole.tools.mgo.model.MultiGoalLike

trait IndividualMGFactory[MG <: MultiGoalLike,G <: AbstractGenome,I <: IndividualMG[G,MG]] {
  def operate(genome:G):I
}

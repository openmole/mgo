/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.domination

import org.openmole.tools.mgo.model.MultiGoalLike

class NonStrictDominant extends Dominant{

  def isDominated(p1: MultiGoalLike, p2: MultiGoalLike): Boolean = {
    for((g1, g2) <- p1.goals zip p2.goals) {
      if(g1.toDouble <= g2.toDouble) return false
    }   
    true
  }
}

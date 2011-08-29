/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.domination

import org.openmole.tools.mgo.model.MultiGoalLike

trait Dominant {
  def isDominated (p1: MultiGoalLike, p2: MultiGoalLike): Boolean
}

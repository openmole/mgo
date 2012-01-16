/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.domination

import org.openmole.tools.mgo.ga.GAFitness

class NonStrictDominant extends Dominant{

  def isDominated(p1: GAFitness, p2: GAFitness): Boolean = {
    for((g1, g2) <- p1.fitness zip p2.fitness) {
      if(g1 <= g2) return false
    }   
    true
  }
}

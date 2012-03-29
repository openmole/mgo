/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.domination

import fr.iscpif.mgo.ga.GAFitness

class NonStrictDominant extends Dominant{

  def isDominated(p1: GAFitness, p2: GAFitness): Boolean = {
    for((g1, g2) <- p1.values zip p2.values) {
      if(g1 <= g2) return false
    }   
    true
  }
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.dominance

import fr.iscpif.mgo._

trait NonStrictDominance extends Dominance {

  def isDominated(p1: Fitness, p2: Fitness): Boolean = {
    for((g1, g2) <- p1.values zip p2.values) {
      if(g1 <= g2) return false
    }   
    true
  }
}

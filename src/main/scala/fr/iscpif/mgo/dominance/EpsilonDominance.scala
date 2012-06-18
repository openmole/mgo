/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.dominance

import fr.iscpif.mgo.Fitness

trait EpsilonDominance extends Dominance { 

  def epsilons: Seq[Double]
  
  def isDominated(p1: Fitness, p2: Fitness): Boolean = {
    for(((g1, g2), e) <- p1.values zip p2.values zip epsilons) {
      if(g1 + e < g2) return false
    }   
    true
  }
}
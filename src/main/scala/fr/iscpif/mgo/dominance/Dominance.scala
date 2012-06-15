/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.dominance

import fr.iscpif.mgo._

trait Dominance { 
  def isDominated (p1: Fitness, p2: Fitness): Boolean
}

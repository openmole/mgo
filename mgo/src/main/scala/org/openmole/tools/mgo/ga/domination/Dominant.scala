/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.domination


import org.openmole.tools.mgo.ga.GAFitness

trait Dominant {
  def isDominated (p1: GAFitness, p2: GAFitness): Boolean
}

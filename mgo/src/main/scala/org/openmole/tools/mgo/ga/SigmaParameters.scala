/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

trait SigmaParameters { self : GAGenome => 
  def sigmaPosition = self.position + sigma.size
  def sigma: Array[Double]
  //override def position = self.position + sigma.size
  //abstract override def values =  self.values ++ sigma  
}

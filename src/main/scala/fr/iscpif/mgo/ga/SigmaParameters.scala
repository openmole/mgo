/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

trait SigmaParameters { this : GAGenome => 
  def sigma: IndexedSeq[Double]
}

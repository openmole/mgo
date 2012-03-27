/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.algorithm


import fr.iscpif.mgo.ga.GAGenome
import fr.iscpif.mgo.ga.SigmaParameters

abstract class GAGenomeWithSigma(val size: Int) extends GAGenome with SigmaParameters {
  def values = wrappedValues.slice(0, size)
  def sigma = wrappedValues.slice(size, size * 2)
}




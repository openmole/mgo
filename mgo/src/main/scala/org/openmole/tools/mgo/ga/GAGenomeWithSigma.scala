/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga


abstract class GAGenomeWithSigma(val size: Int) extends GAGenome with SigmaParameters {
  def values = wrappedValues.slice(0, size)
  def sigma = wrappedValues.slice(size, size * 2)
}




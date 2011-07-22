/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.AbstractGenome

abstract class GAGenome (v: Array[Double]) extends AbstractGenome {
  def position = 0
  def wrappedValues = values
  def values: Array[Double] = v
}

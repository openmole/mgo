/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

trait ToDouble {
  def toDouble: Double
  override def toString = toDouble.toString
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

trait SBXParameters { this: GAGenome =>
  def distributionIndex: Double
  def crossoverRate: Double
  def sbxParameters = List(distributionIndex, crossoverRate)
}

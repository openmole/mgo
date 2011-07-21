/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga
import org.openmole.tools.mgo.model._

trait SigmaParametersGenome extends GenomeDecorator {
  def sigmaPosition = super.position + sigma.size
  def sigma: Array[Double]
  override def position = super.position + sigma.size
  abstract override def values =  super.values ++ sigma
   
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.tools
object ScalingEngine{

  def scale(v:Double,max:Double, min:Double,boundaryMax:Double,boundaryMin:Double):Double =  {
    val factor = (boundaryMax - boundaryMin)  / (max - min)
    return (factor * (v - min) + boundaryMin)
  }
}
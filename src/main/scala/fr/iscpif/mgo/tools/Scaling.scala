/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools

object Scaling {

  implicit def double2Scalable(d: Double) = new {
    def scale(min:Double, max:Double) = Scaling.scale(d, 0, 1, min, max)
    def unscale(min: Double, max: Double) = Scaling.scale(d, min, max, 0, 1)
  }
  
  def scale(v:Double, min:Double, max:Double, boundaryMin:Double, boundaryMax:Double) = {
    val factor = (boundaryMax - boundaryMin)  / (max - min)
    (factor * (v - min) + boundaryMin)
  }
  
}
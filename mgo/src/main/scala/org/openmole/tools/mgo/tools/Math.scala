/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.tools
import scala.math.{max, min}
object Math {
  final def clamp(value:Double, min_v:Double, max_v:Double) : Double = 
    max (min (value, max_v), min_v)
}

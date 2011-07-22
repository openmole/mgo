/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.tools

object Math {
  final def clamp(value:Double, min:Double, max:Double) : Double = {
    var result = value
    if(result > max) result = max
    if(result < min) result = min
    result
  }
}

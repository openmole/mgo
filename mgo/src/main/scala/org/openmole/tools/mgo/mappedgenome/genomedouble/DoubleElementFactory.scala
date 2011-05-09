/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.apache.commons.math.random._
import java.util.Random

class DoubleElementFactory(name:String,min:Double,max:Double) {

  def apply(implicit rng:RandomAdaptor):Tuple2[String,Double] = {
    return Tuple2(name, rng.nextDouble * (max - min) + min)
  }
  
  def empty():Tuple2[String,Double] = {
    return Tuple2(name,0.0)
  }
}

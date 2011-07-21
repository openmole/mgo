/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble
import org.apache.commons.math.random._
import java.util.Random

class Interval(name:String, min:Double, max:Double, precision:Int)  {

  def this(name:String, min:Double, max:Double) = this(name, min, max, -1)
  
  def apply(implicit rng:Random):Tuple2[String,Double] = {
    if (precision != -1){
      var nextInt = rng.nextInt(precision)
      
      return Tuple2(name, ((rng.nextDouble * (max - min) + min) / nextInt.toDouble))
    }else
      return Tuple2(name, rng.nextDouble * (max - min) + min)
  }
  
  def empty():Tuple2[String,Double] = {
    return Tuple2(name,0.0)
  }
  
  def bound():Tuple2[String,(Double,Double)]={
    return Tuple2(name,(min,max))
  }

}

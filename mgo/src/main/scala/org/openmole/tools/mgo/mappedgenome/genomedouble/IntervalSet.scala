/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

import java.util.Random
import scala.collection.JavaConversions._

class IntervalSet(listIntervals:Iterable[Interval]){

     def this(it: java.lang.Iterable[Interval]) = this(asScalaIterable(it))
  
     def map(d: (Interval) => (String,Double)) = {  listIntervals.map(x => d(x)) }
     
  }
  
object IntervalSet {
  implicit def intervallDecorator()  = new {

    def generate(listIntervals:IntervalSet)(implicit rng:Random):GenomeDouble = {
      return (new GenomeDouble(listIntervals.map{_.apply(rng)}.toMap))
    }
    
    def generateEmpty(listIntervals:IntervalSet)(implicit rng:Random):GenomeDouble = {
      return (new GenomeDouble(listIntervals.map{_.empty()}.toMap))
    }
  }
  
}

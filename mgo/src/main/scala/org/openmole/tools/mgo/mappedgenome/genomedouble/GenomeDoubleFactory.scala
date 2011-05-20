/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openmole.tools.mgo.mappedgenome.genomedouble

import org.apache.commons.math.random._
import scala.collection.JavaConversions._

//class GenomeDoubleFactory(rng:RandomAdaptor,element:Iterable[DoubleElementFactory]) {
// 
//  def this(rng:RandomAdaptor, it: java.lang.Iterable[DoubleElementFactory]) = this(rng,asScalaIterable(it))
//  
//  def apply:GenomeDouble={
//    return (new GenomeDouble(element.map{_.apply(rng)}.toMap))
//  }
//  
//  def apply(nbGenomes:Integer):Iterable[GenomeDouble]={
//    return ((0 until nbGenomes).map{i => new GenomeDouble(element.map{_.apply(rng)}.toMap) })
//  }
//  
//  def empty():GenomeDouble={
//    return (new GenomeDouble(element.map{_.empty()}.toMap))
//  }
//  
//}ok

class GenomeDoubleFactory(rng:RandomAdaptor, element:IntervalSet) {

  def apply:GenomeDouble={
    return (new GenomeDouble(element.map{_.apply(rng)}.toMap))
  }
  
  def apply(nbGenomes:Integer):Iterable[GenomeDouble]={
    return ((0 until nbGenomes).map{i => new GenomeDouble(element.map{_.apply(rng)}.toMap) })
  }
  
  def empty():GenomeDouble={
    return (new GenomeDouble(element.map{_.empty()}.toMap))
  }
  
}


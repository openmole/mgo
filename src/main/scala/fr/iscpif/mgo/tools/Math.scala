/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools
import scala.math.{max, min}
import math._

object Math {
  
  val epsilon = 1.0e-20
  
  final def clamp(value:Double, min_v:Double, max_v:Double) : Double = 
    max (min (value, max_v), min_v)
  
  def same(i1: Iterable[Double], i2: Iterable[Double]): Boolean = 
    (i1.headOption, i2.headOption) match {
      case(None, None) => true
      case(None, _) => false
      case(_, None) => false
      case(Some(h1), Some(h2)) => if(abs(h2 - h1) < epsilon) same(i1.tail, i2.tail) else false
    }
  
  def allTheSame(i1: Seq[Iterable[Double]], i2: Seq[Iterable[Double]]) = allTheSameSorted(i1.sorted, i2.sorted)
  
  def allTheSameSorted(i1: Seq[Iterable[Double]], i2: Seq[Iterable[Double]]): Boolean = {
    (i1.headOption, i2.headOption) match {
      case(None, None) => true
      case(None, _) => false
      case(_, None) => false
      case(Some(v1), Some(v2)) => if(same(v1, v2)) allTheSameSorted(i1.tail, i2.tail) else false
    }
  }
  
}

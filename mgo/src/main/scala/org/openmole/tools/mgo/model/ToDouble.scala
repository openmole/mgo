/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model

object ToDouble {
  implicit def int2ToDouble(v: Int) = new ToDouble{ override def toDouble = v.toDouble; override def toString = v.toString} 
  implicit def short2ToDouble(v: Short) = new ToDouble{ override def toDouble = v.toDouble; override def toString = v.toString} 
  implicit def long2ToDouble(v: Long) = new ToDouble{ override def toDouble = v.toDouble; override def toString = v.toString} 
  implicit def float2ToDouble(v: Float) = new ToDouble{ override def toDouble = v.toDouble; override def toString = v.toString}
  implicit def double2ToDouble(v: Double) = new ToDouble{ override def toDouble = v.toDouble; override def toString = v.toString}
  implicit def goalToDouble(toD: ToDouble): Double = toD.toDouble
}

trait ToDouble {
  def toDouble: Double
  override def toString = toDouble.toString
}

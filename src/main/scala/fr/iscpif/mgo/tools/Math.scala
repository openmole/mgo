/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    if(i1.isEmpty || i2.isEmpty) false 
    else if(i1.size == 1) allEquals(i1.head, i2)
    else if(i2.size == 1) allEquals(i2.head, i1)
    else if(same(i1.head, i2.head)) allTheSameSorted(i1.tail, i2.tail) else false
  }
  
  
  
  def allEquals(i: Iterable[Double], in: Seq[Iterable[Double]]) = !in.exists(i2 => !same(i, i2))
  
  def centroid(e: Seq[Seq[Double]]) = e.reduce((x, y) => add(x, y)).map{ x => x / e.size }
  
  def add(x: Seq[Double], y: Seq[Double]) = x zip y map { case(x, y) => x + y }
  
  def squareDist(x: Seq[Double], y: Seq[Double]) = x zip y map { case(x,y) => pow(x + y, 2) } sum
  
}

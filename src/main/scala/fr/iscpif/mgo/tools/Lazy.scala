/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools

object Lazy {
  
  def apply[T](f: => T) = new Lazy(f)
  
  implicit def Lazy[T](implicit ord: Ordering[T]): Ordering[Lazy[T]] = Ordering.by(_.apply)
  
}

class Lazy[T](f: => T) {
  lazy val value: T = f
  def apply() = value
}

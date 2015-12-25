/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools

import scalaz.{ Order, Ordering }

object Lazy {

  /**
   * Build a lazy proxy from a function
   *
   * @tparam T the type of the lazily computed value
   * @param f the function to compute the value
   */
  def apply[T](f: => T) = new Lazy(f)

  /** Pattern matching, (evaluates the lazy computation in x) */
  def unapply[T](x: Lazy[T]): Option[T] = Some(x())

  /**
   * Defines ordering on lazy proxies
   */
  implicit def lazyOrdering[T](implicit ord: scala.Ordering[T]): scala.Ordering[Lazy[T]] = scala.Ordering.by(_.apply)

  implicit def lazyOrder[T](implicit OT: Order[T]): Order[Lazy[T]] = new Order[Lazy[T]] {
    def order(x: Lazy[T], y: Lazy[T]): scalaz.Ordering = OT.order(x(), y())
  }
}

/**
 * Proxy for lazy computation of values
 *
 * @tparam T the type of the lazily computed value
 * @param f a function to compute the value
 */
class Lazy[T](f: => T) {

  /** Cache for value memoization */
  lazy val value: T = f

  /** Get the value */
  def apply() = value

  override def equals(o: Any) =
    o match {
      case that: Lazy[_] => that() == this()
      case _ => false
    }

  override def hashCode = this().hashCode()

  override def toString() = value.toString
}


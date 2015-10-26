/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools

object Lazy {

  /**
   * Build a lazy proxy from a function
   *
   * @tparam T the type of the lazily computed value
   * @param f the function to compute the value
   */
  def apply[T](f: => T) = new Lazy(f)

  /**
   * Defines ordering on lazy proxies
   */
  implicit def lazyOrdering[T](implicit ord: Ordering[T]): Ordering[Lazy[T]] = Ordering.by(_.apply)

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

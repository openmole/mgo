/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import math._
import util.Random

trait ZDT4 extends GAProblem {
  def min = List.fill(n)(0.0)
  def max = 1.0 :: List.fill(n - 1)(5.0)

  def apply(x: IndexedSeq[Double], rng: Random) = Vector(f1(x), f2(x))

  def f1(x: IndexedSeq[Double]) = x(0)
  def f2(x: IndexedSeq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
  def g(x: IndexedSeq[Double]) =
    1 + 10 * (n - 1) + (1 until n).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum
}

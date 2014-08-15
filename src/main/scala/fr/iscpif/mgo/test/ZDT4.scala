/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import math._
import util.Random

trait ZDT4 extends GAProblem with MGFitness {
  def n = genomeSize

  def min = Seq.fill(n)(0.0)
  def max = 1.0 :: List.fill(n - 1)(5.0)

  type P = Seq[Double]

  def express(g: Seq[Double], rng: Random) = Seq(f1(g), f2(g))

  override def evaluate(p: P, rng: Random) = p

  def f1(x: Seq[Double]) = x(0)
  def f2(x: Seq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
  def g(x: Seq[Double]) =
    1 + 10 * (n - 1) + (1 until n).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum
}

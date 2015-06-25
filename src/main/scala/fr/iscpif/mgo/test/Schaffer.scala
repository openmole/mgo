package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import math._
import util.Random

// Simple MG Function created by Schaffer for 1985 VEGA paper
trait Schaffer extends GAProblem with MGFitness {

  def min = Seq.fill(genomeSize)(-100000.0)
  def max = Seq.fill(genomeSize)(100000.0)

  type P = Seq[Double]

  override def express(g: Seq[Double], rng: Random) = Seq(f1(g(0)), f2(g(0)))
  override def evaluate(p: P, rng: Random) = p

  def f1(x: Double) = pow(x, 2)
  def f2(x: Double) = pow(x - 2, 2)

}

///*
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
//package fr.iscpif.mgo.test
//
//import fr.iscpif.mgo._
//import math._
//import util.Random
//
//trait ZDT4 extends GAProblem with MGFitness {
//
//  def min = Seq.fill(genomeSize)(0.0)
//  def max = 1.0 :: List.fill(genomeSize - 1)(5.0)
//
//  type P = Seq[Double]
//
//  override def express(g: Seq[Double], rng: Random) = Seq(f1(g), f2(g))
//  override def evaluate(p: P, rng: Random) = p
//
//  def f1(x: Seq[Double]) = x(0)
//  def f2(x: Seq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
//  def g(x: Seq[Double]) =
//    1 + 10 * (genomeSize - 1) + (1 until genomeSize).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum
//}

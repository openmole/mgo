package mgo.test

import mgo.evolution._
import better.files._

object RastriginHDOSE extends App:
  import algorithm._
  import niche._
  import OSE._

  def dimensions = 30

  val hdose: HDOSE = HDOSE(
    mu = 100,
    lambda = 100,
    fitness = (x, _) => Vector(rastrigin.compute(x)),
    limit = Vector(10.0),
    diversityDistance = 10,
    continuous = rastrigin.continuous(dimensions))

  val (finalState, finalPopulation) =
    hdose.
      until(afterGeneration(5000)).
      trace { (s, is) => println("" + s.generation + " " + s.s.size) }.
      eval(new util.Random(42))

  File("/tmp/hdose.csv") write HDOSE.result(hdose, finalState, finalPopulation).map(_.continuous.mkString(",")).mkString("\n")


//object NoisyRastriginOSE extends App {
//
//  import algorithm._
//  import niche._
//  import NoisyOSE._
//
//  def dimensions = 3
//
//  val ose: NoisyOSE[Vector[Double]] = NoisyOSE(
//    mu = 100,
//    lambda = 100,
//    fitness = (rng, x, _) => Vector(rastrigin.compute(x) + rng.nextGaussian() * 0.25),
//    aggregation = Aggregation.average,
//    limit = Vector(10.0),
//    origin =
//      (c, _) =>
//        boundedGrid(
//          lowBound = Vector.fill(dimensions)(-10.0),
//          highBound = Vector.fill(dimensions)(10.0),
//          definition = Vector.fill(dimensions)(100))(c),
//    continuous = rastrigin.continuous(dimensions))
//
//  val (finalState, finalPopulation) =
//    ose.
//      until(afterGeneration(50000)).
//      trace { (s, is) => println("" + s.generation + " " + s.s._1.size) }.
//      eval(new util.Random(42))
//
//  File("/tmp/ose.csv") write NoisyOSE.result(ose, finalState, finalPopulation).map(_.continuous.mkString(",")).mkString("\n")
//}
//
///**
// * Benchmark function used in
// *  Sambridge, M. (2001). Finding acceptable models in nonlinear inverse problems using a neighbourhood algorithm. Inverse Problems, 17(3), 387.
// *  in the paper: f0 = c*d*((e+g)*h + l) ; g not defined -> use f0 = c*d*(e*h + l)
// *
// *  Neighborhood algorithm (with confidence intervals in second paper)
// *    Sambridge, M. (1999). Geophysical inversion with a neighbourhood algorithm—I. Searching a parameter space. Geophysical journal international, 138(2), 479-494.
// *    Sambridge, M. (1999). Geophysical inversion with a neighbourhood algorithm—II. Appraising the ensemble. Geophysical Journal International, 138(3), 727-746.
// *
// */
//object Sambridge2001OSE extends App {
//  import algorithm._
//  import niche._
//  import OSE._
//
//  def dimensions = 5
//  def continuous(size: Int): Vector[C] = Vector.fill(size)(C(-2.0, 2.0))
//
//  def f(x: Vector[Double]): Double = {
//    val (x1, x2, x3, x4, x5) = (x(0), x(1), x(2), x(3), x(4))
//    val a = 2.5 * math.pow(x1 + 0.2, 2.0) + 1.25 * x2 * x2
//    val b = 5.0 * math.pow(x1 - 0.6, 2.0) + math.pow(x2 + 0.15, 2.0)
//    val c = 0.01 * (x1 * x1 + math.pow(x2 - 1.1, 2.0))
//    val d = math.pow(x1 - 1.0, 2.0) + 10.0 * math.pow(x2 - 1.0, 2.0)
//    val e = 5 * (50 * math.pow(x2 - x1 * x1, 2.0) + math.pow(1 - x1, 2.0))
//    val h = math.pow(x1 + 0.7, 2.0)
//    val l = 5 * math.pow(x2 - 0.5, 2.0)
//    val f0loc = c * d * (e * h + l)
//    a * b * math.log(1 + f0loc) + x3 * x3 + x4 * x4 + x5 * x5
//  }
//
//  val ose: OSE = OSE(
//    mu = 100,
//    lambda = 100,
//    fitness = (x, _) => Vector(f(x)),
//    limit = Vector(0.01),
//    origin =
//      (c, _) =>
//        boundedGrid(
//          lowBound = Vector.fill(dimensions)(-2.0),
//          highBound = Vector.fill(dimensions)(2.0),
//          definition = Vector.fill(dimensions)(100))(c),
//    continuous = continuous(dimensions))
//
//  val (finalState, finalPopulation) =
//    ose.
//      until(afterGeneration(5000)).
//      trace { (s, is) => println("" + s.generation + " " + s.s._1.length) }.
//      eval(new util.Random(42))
//
//  File("./test/ose.csv") write OSE.result(ose, finalState, finalPopulation).map(i => (i.continuous ++ Vector(f(i.continuous))).mkString(",")).mkString("\n")
//}

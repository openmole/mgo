package mgo.test

import mgo._
import mgo.contexts._
import freedsl.dsl._

object RastriginOSE extends App {

  import algorithm._
  import niche._
  import OSE._

  val ose = OSE(
    mu = 100,
    lambda = 100,
    fitness = (x, _) => Vector(rastrigin.compute(x)),
    limit = Vector(10.0),
    origin =
      (c, _) =>
        boundedGrid(
          lowBound = Vector(-10.0, -10.0),
          highBound = Vector(10.0, 10.0),
          definition = Vector(100, 100))(c),
    continuous = rastrigin.continuous(2))

  val (finalState, finalPopulation) =
    OSE.run(new util.Random(42)) { imp =>
      import imp._

      toAlgorithm[DSL](ose).
        until(afterGeneration(10000)).
        trace { (s, is) => println(s.generation) }.
        evolution.eval
    }

  println(OSE.result(ose, finalState).map(_.continuous.mkString(",")).mkString("\n"))
}
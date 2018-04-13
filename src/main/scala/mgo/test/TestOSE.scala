package mgo.test

import mgo._
import mgo.contexts._
import freedsl.dsl._
import better.files._

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
          lowBound = Vector.fill(6)(-10.0),
          highBound = Vector.fill(6)(10.0),
          definition = Vector.fill(6)(100))(c),
    continuous = rastrigin.continuous(6))

  val (finalState, finalPopulation) =
    OSE.run(new util.Random(42)) { imp =>
      import imp._

      toAlgorithm[DSL](ose).
        until(afterGeneration(5000)).
        trace { (s, is) => println(s.generation) }.
        evolution.eval
    }

  File("/tmp/ose.csv") write OSE.result(ose, finalState).map(_.continuous.mkString(",")).mkString("\n")
}
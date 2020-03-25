package mgo.test

import mgo.evolution._

object RastriginNSGA3 extends App {

  import algorithm._

  val nsga3 = NSGA3(
    popSize = 40,
    referencePoints = NSGA3Operations.ReferencePoints(4, 4),
    fitness = (x, _) => Vector(rastrigin.compute(x)),
    continuous = rastrigin.continuous(4))

  def evolution =
    nsga3.until(afterGeneration(1000)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA3.result(nsga3, finalPopulation).mkString("\n"))
}

package mgo.test

import mgo.evolution.*

object NichedNSGAII extends App {

  import algorithm.*

  case class Phenotype(diversity: Double, optimisation: Double)

  val nsga2 = Profile(
    lambda = 100,
    fitness = discreteSphere.compute,
    continuous = discreteSphere.continuous(6),
    discrete = discreteSphere.discrete(3),
    niche = i => CDGenome.discreteValues(discreteSphere.discrete(3)).get(i.genome).take(2).toSeq)

  def evolution =
    nsga2.
      until(afterGeneration(100)).
      trace((s, is) => println(s.generation))

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(Profile.result(nsga2, finalPopulation).mkString("\n"))

}


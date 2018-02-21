package mgo.test

import mgo._
import mgo.contexts._
import freedsl.dsl._

object NichedNSGAII extends App {

  import algorithm._

  case class Phenotype(diversity: Double, optimisation: Double)

  val nsga2 = Profile(
    lambda = 100,
    fitness = discreteSphere.compute,
    continuous = discreteSphere.continuous(6),
    discrete = discreteSphere.discrete(3),
    niche = i => i.genome.discreteValues.take(2).toSeq)

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    nsga2.
      until(afterGeneration(100)).
      trace((s, is) => println(s.generation)).
      evolution
  }

  val (finalState, finalPopulation) =
    Profile.run(new util.Random(42)) { imp =>
      import imp._
      evolution[DSL].eval
    }

  println(Profile.result(nsga2, finalPopulation).mkString("\n"))

}


package mgo.test

import mgo.evolution._
import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.Individual
import mgo.evolution.algorithm.CDGenome.Genome
import mgo.evolution.algorithm.NSGA3Operations

/**
  * For test manyobjective: coco-biobj http://numbbo.github.io/coco-doc/bbob-biobj/functions/ (check mgo-benchmark)
  * Chek Cheng, R., Li, M., Tian, Y., Zhang, X., Yang, S., Jin, Y., & Yao, X. (2017). A benchmark test suite for evolutionary many-objective optimization. Complex & Intelligent Systems, 3(1), 67-81.
  */
object ReferencePoints extends App {

  val divisions = 4
  val dimension = 3

  println("Computing simplex reference points with " + divisions + " divisions in dimension " + dimension)
  val start = System.currentTimeMillis()
  val points = NSGA3Operations.simplexRefPoints(divisions, dimension)
  println("n = " + dimension + " ; p = " + divisions + " ; t = " + (System.currentTimeMillis() - start))
  println(points)

}

object RastriginNSGA3 extends App {

  import algorithm._

  val nsga3 = NSGA3(
    popSize = 40,
    referencePoints = NSGA3Operations.ReferencePoints(20, 2),
    fitness = (x, _) => Vector(rastrigin.compute(x), rastrigin.compute(x.map { _ + 0.5 })),
    continuous = rastrigin.continuous(4))

  def evolution: RunAlgorithm[NSGA3, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    nsga3.until(afterGeneration(100)).
      trace { (s, is) => println(s.generation) }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  println(NSGA3.result(nsga3, finalPopulation).mkString("\n"))
}

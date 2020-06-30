package mgo.test

import java.io.{ BufferedWriter, File, FileWriter }

import mgo.evolution._
import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.Individual
import mgo.evolution.algorithm.CDGenome.Genome
import mgo.evolution.algorithm.NSGA3Operations
import mgo.tools.benchmark.ManyObjective

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

object FunctionNSGA3 extends App {

  import algorithm._

  //def fitness(cont: Vector[Double], discr: Vector[Int]): Vector[Double] = Vector(rastrigin.compute(cont), rastrigin.compute(cont.map { _ + 0.5 }))
  // ! dimension must be correct with reference points
  def fitness(cont: Vector[Double], discr: Vector[Int]): Vector[Double] = ManyObjective.maf1(12)(cont)

  //val ref = NSGA3Operations.ReferencePoints(40, 2)
  val ref = NSGA3Operations.ReferencePoints(40, 3)

  //val genome = rastrigin.continuous(4)
  val genome = Vector.fill(13)(C(0.0, 1.0))

  def export(gen: Long, pop: Vector[Individual[Vector[Double]]]): Unit = {
    val w = new BufferedWriter(new FileWriter(new File("test/pop" + gen + ".csv")))
    w.write(pop.map(i => fitness(i.genome.continuousValues.toVector, Vector.empty)).map(_.mkString(";")).mkString("\n"))
    w.close()
  }

  val nsga3 = NSGA3(
    popSize = 400,
    referencePoints = ref,
    fitness = fitness,
    continuous = genome)

  def evolution: RunAlgorithm[NSGA3, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    nsga3.until(afterGeneration(1000)).
      trace { (s, individuals) =>
        println("\n====================\ngen: " + s.generation)
        export(s.generation, individuals)
      }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

  val res = NSGA3.result(nsga3, finalPopulation)
  println(res.mkString("\n"))
  val fitnesses = res.map(_.fitness.toArray).toArray
  val w = new BufferedWriter(new FileWriter(new File("test/functionNSGA3.csv")))
  w.write(fitnesses.map(_.mkString(";")).mkString("\n"))
  w.close()
  val wr = new BufferedWriter(new FileWriter(new File("test/reference.csv")))
  wr.write(ref.references.map(_.mkString(";")).mkString("\n"))
  wr.close()
}
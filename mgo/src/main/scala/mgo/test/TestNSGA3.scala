package mgo.test

import java.io.{ BufferedWriter, File, FileWriter }

import mgo.evolution._
import mgo.evolution.algorithm.CDGenome.DeterministicIndividual.Individual
import mgo.evolution.algorithm.CDGenome.{ Genome, NoisyIndividual }
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

object ManyObjectiveFunctions extends App {

  /*
  val xt = Vector.fill(10)(0.5)
  def f(x: Double, y: Double): Vector[Double] = ManyObjective.maf1(12)(Vector(x, y) ++ xt)

  println(f(0, 0))
  println(f(1, 1))
  println(f(1, 0))
*/
  val rng = util.Random
  val pop1 = Vector(Vector(rng.nextDouble, rng.nextDouble))
  def fitness(x: Vector[Double]): Vector[Double] = x
  //println(NSGA3Operations.successiveFronts(pop1, fitness))
  //println(NSGA3Operations.successiveFronts(pop1 ++ pop1, fitness))
  //println(NSGA3Operations.successiveFronts(pop1 ++ pop1 ++ pop1, fitness))

  def pop(n: Int) = Vector.fill(n, 2)(rng.nextDouble)
  //println(pop(5))
  (0 until 10).foreach { i =>
    println(i + ": " + NSGA3Operations.successiveFronts(pop(i), fitness))
  }

}

object FunctionNSGA3 extends App {

  import algorithm._

  //def fitness(cont: Vector[Double], discr: Vector[Int]): Vector[Double] = Vector(rastrigin.compute(cont), rastrigin.compute(cont.map { _ + 0.5 }))
  // ! dimension must be correct with reference points
  def fitness(cont: Vector[Double], discr: Vector[Int]): Vector[Double] = ManyObjective.maf1(12)(cont)

  //val ref = NSGA3Operations.ReferencePoints(40, 2)
  val ref = NSGA3Operations.ReferencePoints(50, 3)

  //val genome = rastrigin.continuous(4)
  val genome = Vector.fill(13)(C(0.0, 1.0))

  def write(gen: Long, pop: Vector[Individual[Vector[Double]]]): Unit = {
    val w = new BufferedWriter(new FileWriter(new File("test/pop" + gen + ".csv")))
    w.write(pop.map(i => fitness(i.genome.continuousValues.toVector, Vector.empty)).map(_.mkString(";")).mkString("\n"))
    w.close()
  }

  val nsga3 = NSGA3(
    popSize = 1000,
    referencePoints = ref,
    fitness = fitness,
    continuous = genome)

  def evolution: RunAlgorithm[NSGA3, Individual[Vector[Double]], Genome, EvolutionState[Unit]] =
    nsga3.until(afterGeneration(1000)).
      trace { (s, individuals) =>
        println("\n====================\ngen: " + s.generation)
        write(s.generation, individuals)
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

object TestNoisyNSGA3 extends App {

  import algorithm._

  def fitness(rng: util.Random, cont: Vector[Double], discr: Vector[Int]): Vector[Double] = {
    val res = ManyObjective.maf1(12)(cont).map(_ + 0.1 * rng.nextGaussian())
    //println(res.size)
    res
  }
  // ! for noisy, ref points must be dim + 1
  val ref = NSGA3Operations.ReferencePoints(50, 4)
  val genome = Vector.fill(13)(C(0.0, 1.0))

  val nsga3 = NoisyNSGA3[Vector[Double]](
    popSize = 100,
    referencePoints = ref,
    fitness = fitness,
    // ! take average across repetitions and not within each ! => add assert that dim ref = dim extended obj?
    aggregation = pop => pop.transpose.map(x => x.sum / x.length),
    continuous = genome)

  def evolution: RunAlgorithm[NoisyNSGA3[Vector[Double]], NoisyIndividual.Individual[Vector[Double]], Genome, NoisyNSGA3.NSGA3State] =
    nsga3.until(afterGeneration(10)).
      trace { (s, individuals) =>
        println("\n====================\ngen: " + s.generation)
      }

  val (finalState, finalPopulation) = evolution.eval(new util.Random(42))

}


package mgo.test.bench

import mgo._
import mgo.test._
import mgo.contexts._
import freedsl.dsl._
import cats.implicits._

object BenchRastriginAverageNSGAII extends App {

  import breeding._
  import algorithm._
  import algorithm.nsga2._
  import better.files._

  val directory = File("/tmp/rastrigin_avg/")
  directory.delete(swallowIOExceptions = true)
  directory.createDirectories()

  val rng = new util.Random(42)

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val (nsga2, source) = sourceOf {
      def fitness(x: Vector[Double]) = {
        val samples = Vector.fill(100)(additiveGaussianNoise(0.25, rastrigin.compute).apply(rng, x).head)
        Vector(average(samples))
      }

      NSGA2[M](
        mu = 50,
        lambda = 50,
        fitness = fitness(_),
        genomeSize = 10,
        operators = ManualOperators[M](sbxC(0.5), bga(mutationRate = _ => 1.0, mutationRange = 0.1))
      )
    }

    def save(generation: Long, population: Vector[Individual]) = {
      val lines = result(population, dropWave.scale).map { case (g, v) => g ++ v }.map(_.mkString(","))
      (directory / s"$generation.csv") overwrite lines.mkString("\n")
      (directory / "best") appendLines (Seq.fill(100)(population.map(_.fitness.head).min.toString): _*)
    }

    (directory / "source") overwrite source

    nsga2.
      until(afterGeneration(100)).
      trace((s, is) => save(s.generation, is)).
      evolution
  }

  val (finalState, finalPopulation) = NSGA2(new util.Random(42)) { impl =>
    import impl._
    evolution[DSL].eval
  }

}

object BenchRastriginNoisyNSGAII extends App {

  import breeding._
  import algorithm._
  import algorithm.noisynsga2._
  import better.files._

  val directory = File("/tmp/rastrigin_noisy/")
  directory.delete(swallowIOExceptions = true)
  directory.createDirectories()

  def evolution[M[_]: Generation: Random: cats.Monad: StartTime: IO] = {
    val (nsga2, source) = sourceOf {
      NoisyNSGA2[M](
        mu = 50,
        lambda = 50,
        fitness = additiveGaussianNoise(0.25, rastrigin.compute),
        genomeSize = 2,
        aggregation = averageAggregation(_),
        operators = ManualOperators[M](sbxC(0.5), bga(mutationRate = _ => 1.0, mutationRange = 0.1))
      )
    }

    def save(generation: Long, population: Vector[Individual]) = {
      val lines = {
        val res = population.map { i =>
          dropWave.scale(i.genome.values.toVector) ++ i.fitnessHistory.transpose.head
        }
        res.map(_.mkString(","))
      }

      (directory / s"$generation.csv") overwrite lines.mkString("\n")

      def best = {
        val full =
          population.map { i =>
            i.fitnessHistory.transpose.head
          }.filter(_.size == 100)

        full.isEmpty match {
          case true => Double.PositiveInfinity
          case false => full.map(v => average(v.toVector)).min
        }
      }

      (directory / "best") appendLine best.toString
    }

    (directory / "source") overwrite source

    nsga2.
      until(afterGeneration(10000)).
      trace((s, is) => save(s.generation, is)).
      evolution
  }

  val (finalState, finalPopulation) = NoisyNSGA2.run(new util.Random(42)) { impl =>
    import impl._
    evolution[DSL].eval
  }

}
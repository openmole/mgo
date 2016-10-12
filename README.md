MGO
===

MGO is a purely functionnal scala library based for evolutionary / genetic algorithms:
* enforcing immutability,
* exposes a modular and extensible architecture,
* implements state of the art algorithms,
* handles noisy (stochastic) fitness functions,
* implements auto-adaptatative algortihms.

MGO implements NGSAII, CP (Calibration Profile), PSE (Pattern Search Experiment).

Licence
-------

MGO is licenced under the GNU Affero GPLv3 software licence. 

Example
-------

Define a problem, for instance the multi-modal multi-objective ZDT4 benchmark:

```scala

  object zdt4 {

    def scale(s: Vector[Double]): Vector[Double] = s.map(_.scale(0.0, 5.0))

    def compute(genome: Vector[Double]): Vector[Double] = {
      val genomeSize = genome.size

      def g(x: Seq[Double]) =
        1 + 10 * (genomeSize - 1) +
          x.map { i => pow(i, 2) - 10 * cos(4 * Pi * i) }.sum

      def f(x: Seq[Double]) = {
        val gx = g(x)
        gx * (1 - sqrt(genome(0) / gx))
      }

      val scaled = scale(genome)
      Vector(scaled(0), f(scaled.tail))
    }

  }

```

Define the optimisation algorithm, for instance NSGAII:

```scala

  val nsga2 =
    NSGA2(
      mu = 100,
      lambda = 100,
      fitness = zdt4.compute,
      genomeSize = 10)

```

Run the optimisation:

```scala

  val (finalState, finalPopulation) =
    run(nsga2).
      until(afterGeneration(1000)).
      trace((state, population) => println(state.generation)).
      eval(new Random(42))

  println(result(finalPopulation, zdt4.scale).mkString("\n"))

```

Noisy fitness functions
-----------------------

All algorithm in MGO have version to compute on noisy fitness function. MGO handle noisy fitness functions by resampling
only the most promising individuals. It uses an aggregation function to aggregate the multiple sample when needed.

For instance a version of NSGA2 for noisy fitness functions may be used has follow:

```scala
  import algorithm.noisynsga2._
  import scala.util.Random

  object sphere {
    def scale(s: Vector[Double]): Vector[Double] = s.map(_.scale(-2, 2))
    def compute(i: Vector[Double]): Double = i.map(x => x * x).sum
  }

  object noisySphere {
    def scale(s: Vector[Double]): Vector[Double] = sphere.scale(s)
    def compute(rng: Random, v: Vector[Double]) =
      Vector(sphere.compute(v) + rng.nextGaussian() * 0.5 * math.sqrt(sphere.compute(v)))
  }

  def aggregation(history: Vector[Vector[Double]]) = history.transpose.map { o => o.sum / o.size }

  val nsga2 =
    NoisyNSGA2(
      mu = 100,
      lambda = 100,
      fitness = noisySphere.compute,
      aggregation = aggregation,
      genomeSize = 2)

  val (finalState, finalPopulation) =
    run(nsga2).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new Random(42))

  println(result(finalPopulation, aggregation, noisySphere.scale).mkString("\n"))
```

Diversity only
--------------

MGO proposes the PSE alorithm that aim a creating diverse solution instead of optimsing a function. The paper about this
algorithm can be found [here](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0138212).

```scala
  import fr.iscpif.mgo._
  import algorithm.pse._
  import util.Random

  val pse = PSE(
    lambda = 10,
    phenotype = zdt4.compute,
    pattern =
      boundedGrid(
        lowBound = Vector(0.0, 0.0),
        highBound = Vector(1.0, 200.0),
        definition = Vector(10, 10)),
    genomeSize = 10)

  val (finalState, finalPopulation) =
    run(pse).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new Random(42))

  println(result(finalPopulation, zdt4.scale).mkString("\n"))
```

This program explores all the different combination of values that can be produced by the multi-objective function of ZDT4.

For more examples, have a look at the main/scala/fr/iscpif/mgo/test directory in the repository.

Mixed optimisation and diversity
--------------------------------

The calibration profile algorthim compute the best fitness function for a set of niches. This algorithm is explained [here](http://jasss.soc.surrey.ac.uk/18/1/12.html).

In MGO you can compute profiles of a 10 dimensional hyper-sphere function using the following:

```scala

  import algorithm.profile._
  import util.Random

  //Profile the first dimension of the genome
  val algo = Profile(
    lambda = 100,
    fitness = sphere.compute,
    niche = genomeProfile(x = 0, nX = 10),
    genomeSize = 10)

  val (finalState, finalPopulation) =
    run(algo).
      until(afterGeneration(1000)).
      trace((s, is) => println(s.generation)).
      eval(new Random(42))

  println(result(finalPopulation, sphere.scale).mkString("\n"))
```



Distributed computing
---------------------

Algorithms implemented in MGO are also avialiable in the workflow plateform for distributed computing [OpenMOLE](http://openmole.org).
  
SBT dependency
----------------

    libraryDependencies += "fr.iscpif" %% "mgo" % "version"


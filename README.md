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

  def zdt4 = new Problem[Vector[Double], Vector[Double]] {

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

Diversity only
--------------

MGO proposes the PSE alorithm that aim a creating diverse solution instead of optimsing a function. The paper about this
algorithm can be found (here)[http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0138212].

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


For more examples, have a look at the main/scala/fr/iscpif/mgo/test directory in the repository.

Distributed computing
---------------------

Algorithms implemented in MGO are also avialiable in the workflow plateform for distributed computing (OpenMOLE)[http://openmole.org].
  
SBT dependency
----------------

    libraryDependencies += "fr.iscpif" %% "mgo" % "version"


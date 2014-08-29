MGO
===

MGO is a library based on the cake pattern for multi-objective evolutionary algorithm:
* written in scala,
* enforcing immutability,
* exposes a modular and extensible architecture,
* implements state of the art algorithms,
* take advantage of multi-core architectures.

MGO implements NGSAII, SMSEMOEA, CMAES and other diversity based evolutionary algorithms.

Licence
-------

MGO is licenced under the GNU Affero GPLv3 software licence. 

Example
-------

Define a problem, for instance ZDT4:

```scala
import fr.iscpif.mgo._
import math._
import util.Random

trait ZDT4 extends GAProblem with MGFitness {

  def min = Seq.fill(genomeSize)(0.0)
  def max = 1.0 :: List.fill(genomeSize - 1)(5.0)

  type P = Seq[Double]

  override def express(g: Seq[Double], rng: Random) = Seq(f1(g), f2(g))
  override def evaluate(p: P, rng: Random) = p

  def f1(x: Seq[Double]) = x(0)
  def f2(x: Seq[Double]) = g(x) * (1 - sqrt(x(0) / g(x)))
  def g(x: Seq[Double]) =
    1 + 10 * (genomeSize - 1) + (1 until genomeSize).map { i => pow(x(i), 2) - 10 * cos(4 * Pi * x(i)) }.sum
}
```

Define the optimisation algorithm, for instance NSGAII:

```scala
  val m =
    new ZDT4 with NSGAII with CounterTermination {
      def steps = 1000
      def mu = 200
      def lambda = 200
      def genomeSize = 10
    }
```

Run the optimisation:

```scala
  implicit val rng = newRNG(42)

  val res =
    m.evolve.untilConverged {
      s => println(s.generation)
    }

  val output = Resource.fromFile("/tmp/res.csv")
  for {
    r <- res.population.toIndividuals
  } {
    def line = m.scale(m.values.get(r.genome)) ++ m.fitness(r)
    output.append(line.mkString(",") + "\n")
  }
```

For more examples, have a look at the main/scala/fr/iscpif/mgo/test directory in the repository.
  
SBT dependency
----------------

    libraryDependencies += "fr.iscpif" %% "mgo" % "version"


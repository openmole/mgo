/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import fr.iscpif.mgo.algorithm.ga._
import fitness._
import genome._
import cloneOld._

import scala.util.Random
import scalaz._
import Scalaz._

import scalaz.effect.IO

object SphereNSGAII extends App {
  import Algorithms.NSGA2
  import Contexts.default._
  import Contexts._
  import Expressions._

  val mu = 100
  val lambda = 100
  def dimensions = 10
  val maxiter = 100000

  def express: Expression[NSGA2.Genome, Vector[Double]] = { case NSGA2.Genome(values, _, _) => Vector(sphere(values)) }

  val ea: Vector[NSGA2.Individual] => EvolutionState[Unit, Vector[NSGA2.Individual]] =
    runEAUntil[NSGA2.Individual, EvolutionStateMonad[Unit]#l, NSGA2.Genome](
      stopCondition = { (individuals: Vector[NSGA2.Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[NSGA2.Individual]) =>
          for {
            generation <- implicitly[Generational[EvolutionStateMonad[Unit]#l]].getGeneration
            _ <- implicitly[MonadTrans[({ type L[f[_], a] = StateT[f, EvolutionData[Unit], a] })#L]].liftMU(IO.putStrLn(s"Generation ${generation.toString} Best " ++ individuals.minBy { _.fitness.sum }.toString))
            res <- NSGA2.step[EvolutionStateMonad[Unit]#l](fitness = express, mu = mu, lambda = lambda)(implicitly[Monad[EvolutionStateMonad[Unit]#l]], implicitly[RandomGen[EvolutionStateMonad[Unit]#l]], implicitly[Generational[EvolutionStateMonad[Unit]#l]])(individuals)
          } yield res
    )

  val evolution: EvolutionState[Unit, Vector[NSGA2.Individual]] =
    for {
      initialGenomes <- NSGA2.initialGenomes[EvolutionStateMonad[Unit]#l](mu, dimensions)
      initialPop = initialGenomes.map { (g: NSGA2.Genome) => NSGA2.Individual(g, express(g)) }
      finalpop <- ea(initialPop)
    } yield finalpop

  val (finalstate, finalpop) = evolution(EvolutionData[Unit](random = newRNG(1), s = ())).unsafePerformIO()

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop)

  println("---- Fitnesses ----")
  println(finalpop.map { (_: NSGA2.Individual).fitness })
}

object SphereNSGAIIOld extends App {

  def dimensions = 10
  def problem(g: GAGenome) = State { rng: Random => (rng, sphere(g.genomeValue)) }

  val evo =
    evolution(
      NSGA2[Double](
        mu = 10,
        fitness = i => Seq(i.phenotype)
      )
    )(
        100,
        randomGenome(dimensions),
        problem,
        afterGeneration(100)
      )

  println(evo.eval(42).minBy(_.phenotype))

}

object StochasticSphereNSGAII extends App {
  def average(s: Seq[Double]) = s.sum / s.size

  val algo =
    noisyNSGA2[Double](
      mu = 100,
      fitness = i => Seq(average(i.phenotype)),
      history = 100
    )

  def dimensions = 10
  def function = rastrigin

  def problem(g: GAGenome) = State { rng: Random =>
    val scaled = function.scale(g.genomeValue)
    val eval = function.compute(scaled)
    (rng, eval + (rng.nextGaussian() * 0.5 * math.sqrt(eval)))
  }

  val evo = evolution(algo)(100, randomGenome(dimensions), problem, afterGeneration(10000))

  import scala.Ordering.Implicits._
  val (s, res) = evo.run(47)

  val oldest = res.minBy(i => Seq(-i.phenotype.size, average(i.phenotype)))

  println(res.count(_.phenotype.size == oldest.phenotype.size))
  println(res.groupBy(_.genome.fromOperation).toList.map { case (k, v) => k -> v.size }.sortBy(_._1))
  println(function.scale(oldest.genomeValue) + " " + average(oldest.phenotype) + " " + oldest.phenotype.size + " " + oldest.born)

}

/*
 * Copyright (C) 08/01/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.test

import fr.iscpif.mgo._
import fr.iscpif.mgo.niche._

import algorithm.NoisyProfile
import Contexts.default._
import Contexts._
import Expressions._

import scala.util.Random
import scalaz._
import Scalaz._

import scalaz.effect.IO

object SphereProfile extends App {

  import algorithm.Profile._

  val muByNiche = 1
  val lambda = 10
  val dimensions = 10
  val maxiter = 100
  val operationExploration = 0.1

  def express: Expression[Genome, Double] = { case Genome(values, _, _) => sphere(values) }

  //Niche over the first dimension of the genome
  def niche: Niche[Individual, Int] = genomeProfile[Individual](
    values = (_: Individual).genome.values,
    x = 0,
    nX = 10)

  val ea: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
    runEAUntil[Individual, EvolutionStateMonad[Unit]#l](
      stopCondition = { (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[Individual]) => for {
          _ <- liftIOValue(writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue(write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitness }.toString))
          res <- step[EvolutionStateMonad[Unit]#l](
            fitness = express,
            niche = niche,
            muByNiche = muByNiche,
            lambda = lambda,
            operationExploration = operationExploration)(implicitly[Monad[EvolutionStateMonad[Unit]#l]], implicitly[RandomGen[EvolutionStateMonad[Unit]#l]], implicitly[Generational[EvolutionStateMonad[Unit]#l]])(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      initialGenomes <- initialGenomes[EvolutionStateMonad[Unit]#l](muByNiche, dimensions)
      initialPop = initialGenomes.map { (g: Genome) => Individual(g, express(g)) }
      finalpop <- ea(initialPop)
    } yield finalpop

  val start = Contexts.default.wrap[Unit, Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = Contexts.default.unwrap[Unit, Vector[Individual]](s = ())(
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: Individual).fitness }.mkString("\n"))

}

object StochasticSphereProfile extends App {

  import algorithm.NoisyProfile._

  val muByNiche = 1
  val lambda = 10
  def dimensions = 10
  val maxiter = 1000
  val operationExploration = 0.1
  val cloneProbability = 0.1
  val historySize = 100

  //TODO: Stochastic Sphere
  def express: Expression[(Random, Vector[Double]), Double] = { case (rg: Random, v: Vector[Double]) => rg.nextGaussian() * 0.5 + math.sqrt(sphere(v)) }

  //Niche over the first dimension of the genome
  def niche: Niche[Individual, Int] = genomeProfile[Individual](
    values = (_: Individual).genome,
    x = 0,
    nX = 10)

  val ea: Vector[Individual] => EvolutionState[Unit, Vector[Individual]] =
    runEAUntil[Individual, EvolutionStateMonad[Unit]#l](
      stopCondition = { (individuals: Vector[Individual]) =>
        implicitly[Generational[EvolutionStateMonad[Unit]#l]].generationReached(maxiter)
      },
      stepFunction =
        (individuals: Vector[Individual]) => for {
          _ <- liftIOValue[Unit, Unit](writeGen[EvolutionStateMonad[Unit]#l]())
          _ <- liftIOValue[Unit, Unit](write[EvolutionStateMonad[Unit]#l](individuals.minBy { _.fitnessHistory.last }.toString))
          res <- step[EvolutionStateMonad[Unit]#l](
            fitness = express,
            niche = niche,
            muByNiche = muByNiche,
            lambda = lambda,
            historySize = historySize,
            operationExploration = operationExploration,
            cloneProbability = cloneProbability)(implicitly[Monad[EvolutionStateMonad[Unit]#l]], implicitly[RandomGen[EvolutionStateMonad[Unit]#l]], implicitly[Generational[EvolutionStateMonad[Unit]#l]])(individuals)
        } yield res
    )

  val evolution: EvolutionState[Unit, Vector[Individual]] =
    for {
      rg <- implicitly[RandomGen[EvolutionStateMonad[Unit]#l]].split
      initialPop <- initialPopulation[EvolutionStateMonad[Unit]#l](muByNiche, dimensions)
      initialPopEval = initialPop.map { (i: Individual) => individualHistory.append(i, express((rg, i.genome))) }
      finalpop <- ea(initialPopEval)
    } yield finalpop

  val start = Contexts.default.wrap[Unit, Unit](EvolutionData[Unit](random = newRNG(1), s = ()), ())

  val (finalstate, finalpop) = Contexts.default.unwrap[Unit, Vector[Individual]](s = ())(
    start >> evolution
  )

  println("---- Final State ----")
  println(finalstate)

  println("---- Final Population ----")
  println(finalpop.mkString("\n"))

  println("---- Fitnesses ----")
  println(finalpop.map { (_: Individual).fitnessHistory }.mkString("\n"))

}

//object Profile {
//  val deterministic =
//    profile[Double](
//      fitness = Fitness(_.phenotype),
//      niche = genomeProfile(0, 100)
//    )()
//
//
//  val stochastic =
//    profile[History[Double]](
//      fitness = Fitness(i => average(i.phenotype)),
//      niche = genomeProfile(0, 100)
//    )(cloneStrategy = queue(100))
//
//}
//
//object SphereProfile extends App {
//
//  def dimensions = 10
//  def problem(g: GAGenome) = State.gets { rng: Random => sphere(g.genomeValue) }
//
//  val evo = evolution(Profile.deterministic, 100)(randomGenome(dimensions), problem, afterGeneration(1000))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}
//

/*object StochasticSphereProfileOld extends App {

  import fr.iscpif.mgo._
  import algorithm.ga._
  import genome._

  import scalaz._
  import util.Random

  import nicheOld._
  import cloneOld._

  //Niche over the first dimension of the genome
  def niche = genomeProfile[GAGenome](0, 100)

  val stochastic =
    noisyProfile[Double](
      fitness = i => average(i.phenotype),
      niche = niche,
      nicheSize = 10,
      history = 100
    )

  def dimensions = 10
  def problem(g: GAGenome) = State.gets { rng: Random => sphere(g.genomeValue) + (rng.nextDouble() * 0.5) }

  val evo = evolution(stochastic)(100, randomGenome(dimensions), problem, afterGeneration(10000), s => println(s.common.generation))
  val res = evo.eval(42)

  val bests =
    for {
      p <- res.groupBy(i => niche(i)).map(_._2)
    } yield p.maxBy(_.phenotype.size)

  for { b <- bests } println(b.phenotype.size)
  for { b <- bests } println(s"""${b.genomeValue.mkString(",")},${average(b.phenotype)}""")

}*/
//
//object TestProfileRastrigin extends App {
//
//  import Profile.profile
//  import profile._
//
//  def dimensions = 10
//  def problem(g: G) = State { rng: Random => (rng, rastrigin(genomeValues.get(g))) }
//
//  val evo = evolution(profile, 100)(randomGenome(dimensions), problem, afterGeneration(100))
//  val res =
//    evo.eval(42).map {
//      i => s"${i.genome.values(0)}, ${i.phenotype}"
//    }.mkString("\n")
//
//  println(res)
//
//}
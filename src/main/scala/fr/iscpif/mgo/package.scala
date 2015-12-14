/*
 * Copyright (C) 2012 reuillon, Guillaume Chérel
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

package fr.iscpif

import fr.iscpif.mgo.tools._
import org.apache.commons.math3.random._
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.Random
import scala.concurrent.duration._

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.effect.IO

import mgo.Breedings._
import mgo.Expressions._
import mgo.Objectives._
import fr.iscpif.mgo.Contexts._

package object mgo {

  /**** Running the EA ****/

  def stepEA[I, M[_]: Monad, G](
    preStep: Vector[I] => M[Unit],
    breeding: Breeding[I, M, G],
    expression: Expression[G, I],
    objective: Objective[I, M],
    replacementStrategy: (Vector[I], Vector[I]) => Vector[I]): Vector[I] => M[Vector[I]] =
    (population: Vector[I]) =>
      for {
        _ <- preStep(population)
        bred <- breeding(population)
        expressed = bred.par.map(expression).toVector
        kept <- objective(replacementStrategy(population, expressed))
      } yield kept

  def runEA[I, M[_]: Monad, G](stepFunction: Vector[I] => M[Vector[I]]): Vector[I] => M[Vector[I]] =
    runEAUntil[I, M, G](
      { (_: Vector[I]) => false.point[M] },
      stepFunction)

  /*def runEAUntilNR[I, M[_]: Monad, G](
    stopCondition: Vector[I] => M[Boolean],
    stepFunction: Vector[I] => M[Vector[I]],
    stop: Boolean = false)(population:Vector[I]): M[Vector[I]] =

    var stop = false
    var newpop = population

    while(!stop) {
      for {
        stop_ <- stopCondition(newpop)
        newpop_ <- stepFunction(newpop)
      } yield {
        stop = stop_
        newpop = newpop_
        ()}
    }

    newpop.point[M]
  }*/

  //TODO: Non-tail recursive function. Make a tail recursive one (Trampoline).
  def runEAUntil[I, M[_]: Monad, G](
    stopCondition: Vector[I] => M[Boolean],
    stepFunction: Vector[I] => M[Vector[I]])(population: Vector[I]): M[Vector[I]] =
    (for {
      stop <- stopCondition(population)
    } yield {
      if (stop) population.point[M]
      else for {
        newpop <- stepFunction(population)
        next <- runEAUntil[I, M, G](stopCondition, stepFunction)(newpop)
      } yield next
    }).join

  /**** Stop conditions ****/

  def anyReaches[I, M[_]: Monad](goalReached: I => Boolean)(population: Vector[I]): Vector[I] => M[Boolean] =
    (population: Vector[I]) => population.exists(goalReached).point[M]

  /**** Pre-step functions ****/

  def write[M[_]: Monad](s: String, writeFunction: String => IO[Unit] = IO.putStrLn): M[IO[Unit]] =
    for {
      io <- writeFunction(s).point[M]
    } yield io

  def writeGen[M[_]: Monad: Generational](writeFunction: Long => IO[Unit] = { g => IO.putStrLn(s"Generation ${g.toString}") }): M[IO[Unit]] =
    for {
      generation <- implicitly[Generational[M]].getGeneration
      io <- writeFunction(generation).point[M]
    } yield io

  /**** Replacement strategies ****/

  def muPlusLambda[I](parents: Vector[I], offsprings: Vector[I]): Vector[I] = parents ++ offsprings
  def muCommaLambda[I](parents: Vector[I], offsprings: Vector[I]): Vector[I] = offsprings

  /**** Breeding ****/

  def bindB[I, M[_]: Monad, G1, G2](b1: Breeding[I, M, G1], b2: Vector[G1] => Breeding[I, M, G2]): Breeding[I, M, G2] =
    (individuals: Vector[I]) => for {
      g1s <- b1(individuals)
      g2s <- b2(g1s)(individuals)
    } yield g2s

  def zipB[I, M[_]: Monad, G1, G2](b1: Breeding[I, M, G1], b2: Breeding[I, M, G2]): Breeding[I, M, (G1, G2)] = zipWithB { (g1: G1, g2: G2) => (g1, g2) }(b1, b2)

  def zipWithB[I, M[_]: Monad, G1, G2, G3](f: ((G1, G2) => G3))(b1: Breeding[I, M, G1], b2: Breeding[I, M, G2]): Breeding[I, M, G3] =
    (individuals: Vector[I]) =>
      for {
        g1s <- b1(individuals)
        g2s <- b2(individuals)
      } yield (g1s, g2s).zipped.map(f)

  def productB[I, M[_]: Monad, G1, G2](b1: Breeding[I, M, G1], b2: G1 => Breeding[I, M, G2]): Breeding[I, M, G2] =
    productWithB[I, M, G1, G2, G2] { (_: G1, g2: G2) => g2 }(b1, b2)

  def liftB[I, I1, M[_]: Monad, G1, G](itoi1: I => I1, g1tog: G1 => G, breeding: Breeding[I1, M, G1]): Breeding[I, M, G] =
    (individuals: Vector[I]) =>
      breeding(individuals.map(itoi1)).map[Vector[G]] { (g1s: Vector[G1]) => g1s.map(g1tog) }

  def productWithB[I, M[_]: Monad, G1, G2, G3](f: (G1, G2) => G3)(b1: Breeding[I, M, G1], b2: G1 => Breeding[I, M, G2]): Breeding[I, M, G3] =
    (individuals: Vector[I]) =>
      for {
        g1s <- b1(individuals)
        nested <- g1s.traverse[M, Vector[G3]] { (g1: G1) => b2(g1)(individuals).map { (g2s: Vector[G2]) => g2s.map { (g2: G2) => f(g1, g2) } } }
      } yield Monad[Vector].join(nested)

  def mapB[I, M[_]: Monad, G](mutation: I => M[G]): Breeding[I, M, G] =
    (individuals: Vector[I]) => individuals.traverse[M, G](mutation)

  def byNicheB[I, N, M[_]: Monad, G](niche: I => N)(breeding: Breeding[I, M, G]): Breeding[I, M, G] =
    (individuals: Vector[I]) => {
      val indivsByNiche: Map[N, Vector[I]] = individuals.groupBy(niche)
      indivsByNiche.valuesIterator.toVector.traverse[M, Vector[G]](breeding).map[Vector[G]](_.flatten)
    }

  def probabilisticOperatorB[I, M[_]: Monad: RandomGen, G](
    opsAndWeights: Vector[(I => M[G], Double)]): I => M[G] =
    (mates: I) => {
      for {
        rg <- implicitly[RandomGen[M]].split
        op = multinomial[I => M[G]](opsAndWeights.toList)(rg)
        g <- op(mates)
      } yield g
    }

  /** Breed a genome for subsequent stochastic expression */
  def withRandomGenB[I, M[_]: Monad: RandomGen, G](breeding: Breeding[I, M, G]): Breeding[I, M, (Random, G)] =
    (individuals: Vector[I]) =>
      for {
        bred <- breeding(individuals)
        rgs <- implicitly[RandomGen[M]].split.replicateM(bred.size)
      } yield rgs.toVector zip bred

  /**** Expression ****/

  def bindE[G, P1, P2](e1: Expression[G, P1], e2: P1 => Expression[G, P2]): Expression[G, P2] =
    (genome: G) => e2(e1(genome))(genome)

  def zipE[G, P1, P2](e1: Expression[G, P1], e2: Expression[G, P2]): Expression[G, (P1, P2)] =
    zipWithE[G, P1, P2, (P1, P2)] { (p1: P1, p2: P2) => (p1, p2) }(e1, e2)

  def zipWithE[G, P1, P2, P3](f: (P1, P2) => P3)(e1: Expression[G, P1], e2: Expression[G, P2]): Expression[G, P3] =
    (genome: G) => f(e1(genome), e2(genome))

  def asE[G, G1, P](gtog1: G => G1, express: Expression[G1, P]): Expression[G, P] = (genome: G) => express(gtog1(genome))

  def withGenomeE[G, P](express: Expression[G, P]): Expression[G, (P, G)] = (genome: G) => (express(genome), genome)

  def withE[G, P](f: G => P): Expression[G, P] = f

  /**** Objectives ****/

  def bindO[M[_]: Monad, I](o1: Objective[I, M], o2: Vector[I] => Objective[I, M]): Objective[I, M] =
    (phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(selected1s)(phenotypes)
      } yield selected2s

  def andO[M[_]: Monad, I](o1: Objective[I, M], o2: Objective[I, M]): Objective[I, M] =
    (phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(phenotypes)
      } yield selected1s.intersect(selected2s)

  def orO[M[_]: Monad, I](o1: Objective[I, M], o2: Objective[I, M]): Objective[I, M] =
    (phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(phenotypes)
      } yield selected1s.union(selected2s)

  def thenO[M[_]: Monad, I](o1: Objective[I, M], o2: Objective[I, M]): Objective[I, M] =
    (phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(selected1s)
      } yield selected2s

  def byNicheO[I, N, M[_]: Monad](niche: I => N, objective: Objective[I, M]): Objective[I, M] =
    (individuals: Vector[I]) => {
      val indivsByNiche: Map[N, Vector[I]] = individuals.groupBy(niche)
      indivsByNiche.valuesIterator.toVector.traverse[M, Vector[I]](objective).map[Vector[I]](_.flatten)
    }

  /**** Some typeclasses, move this to a more appropriate place ****/
  trait Age[I] {
    def getAge(i: I): Long
    def setAge(i: I, a: Long): I
  }

  trait PhenotypeHistory[I, P] {
    def getPhenotype(i: I): P
    def append(i: I, p: P): I
    def getHistory(i: I): Vector[P]
    def setHistory(i: I, h: Vector[P]): I
  }

  /***********************************************/

  implicit def common[S] = monocle.macros.Lenser[AlgorithmState[S]](_.common)
  implicit def state[S] = monocle.macros.Lenser[AlgorithmState[S]](_.state)
  implicit def generation[S] = common[S] composeLens CommonState.generation
  implicit def startTime[S] = common[S] composeLens CommonState.startTime
  implicit def random[S] = common[S] composeLens CommonState.random

  type Population[I] = Vector[I]

  object Population {
    def empty = Vector.empty
  }

  implicit def unwrap[@specialized A, T](a: A @@ T): A = Tag.unwrap[A, T](a)
  implicit def wrap[@specialized A, T](a: A): A @@ T = Tag.apply[A, T](a)

  def identityLens[A] = monocle.Lens[A, A](identity)(v => identity)

  //implicit def functionStateConverter[A, B, S](f: A => B) = (a: A) => State.gets { s: S => f(a) }

  private def changeScale(v: Double, min: Double, max: Double, boundaryMin: Double, boundaryMax: Double) = {
    val factor = (boundaryMax - boundaryMin) / (max - min)
    (factor * (v - min) + boundaryMin)
  }

  implicit def double2Scalable(d: Double) = new {
    def scale(min: Double, max: Double) = changeScale(d, 0, 1, min, max)
    def unscale(min: Double, max: Double) = changeScale(d, min, max, 0, 1)
  }

  def newRNG(seed: Long) = new util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(new Well44497a(seed))))
  implicit def longToRandom(seed: Long) = newRNG(seed)

  implicit class LensDecorator[A, B](lens: monocle.Lens[A, B]) {
    def toScalaz = scalaz.Lens.lensu[A, B]((a, b) => lens.set(b)(a), lens.get)
  }

  implicit def monocleToScalazLens[A, B](lens: monocle.Lens[A, B]) = lens.toScalaz

  implicit class VectorStateDecorator[S, G](gen: State[S, Vector[G]]) {

    def generateFlat(lambda: Int) = {
      def flatten0(lambda: Int)(state: S, acc: List[G] = List()): (S, List[G]) =
        if (lambda <= 0) (state, acc)
        else {
          val (newState, add) = gen.map {
            _.take(lambda)
          }.run(state)
          flatten0(lambda - 1)(newState, add.toList ::: acc)
        }

      State { state: S => flatten0(lambda)(state) }.map { _.toVector }
    }

  }

  def updateGeneration[S] =
    for {
      s <- State.get[AlgorithmState[S]]
      _ <- State.put(generation[S].modify(_ + 1)(s))
    } yield {}

  implicit class ElementStateDecorator[S, G](gen: State[S, G]) {
    def generate(lambda: Int) = gen.map(Vector(_)).generateFlat(lambda)
  }

  def randomGenomes[G](randomGenome: State[Random, G], size: Int) = randomGenome.generate(size).map(_.toVector)

  def expressMonad[G, P, S](express: (G => State[Random, P])) = (g: G) => common[S] lifts Individual(g, express)

  def step[G, P, S](algorithm: AlgorithmOld[G, P, S], lambda: Int, express: (G => State[Random, P]))(population: Population[Individual[G, P]]) =
    for {
      breed <- algorithm.breeding(population, lambda)
      offspring <- breed.traverseS(expressMonad[G, P, S](express))
      population <- algorithm.elitism(population, offspring)
      _ <- updateGeneration[S]
    } yield population

  def initialGenomes[G](newGenome: State[Random, G], size: Int) = newGenome.generate(size)

  def evolution[G, P, S](algorithm: AlgorithmOld[G, P, S])(
    lambda: Int,
    newGenome: State[Random, G],
    express: (G => State[Random, P]),
    termination: Termination[AlgorithmState[S]],
    onStep: AlgorithmState[S] => Unit = (_: AlgorithmState[S]) => {}) = new {

    def eval(random: Random): Population[Individual[G, P]] = run(random)._2

    def run(rng: Random) = {
      @tailrec def run0(
        population: Population[Individual[G, P]],
        state: AlgorithmState[S]): (AlgorithmState[S], Population[Individual[G, P]]) = {
        onStep(state)
        val (s1, res) = step(algorithm, lambda, express)(population).run(state)
        val (s2, cond) = termination.run(s1)
        if (cond) (s2, res)
        else run0(res, s2)
      }

      val allRun =
        for {
          genomes <- random[S] lifts initialGenomes(newGenome, lambda)
          initialPop <- genomes.traverseS(expressMonad[G, P, S](express))
          finalPop <- State[AlgorithmState[S], Population[Individual[G, P]]] { state: AlgorithmState[S] => run0(initialPop, state) }
        } yield finalPop

      allRun.run(algorithm.algorithmState(rng))
    }
  }

  @tailrec def group[I](col: List[I], acc: List[List[I]] = List())(equality: Equal[I]): List[List[I]] =
    col match {
      case Nil => acc
      case h :: t =>
        val (begin, end) = acc.span { l => !equality.equal(h, l.head) }
        val newContent = h :: end.headOption.getOrElse(Nil)
        group(t, begin ::: newContent :: end.drop(1))(equality)
    }

  type Termination[S] = State[S, Boolean]

  sealed trait Generation
  sealed trait Start

  def afterGeneration[S](max: Long)(implicit step: monocle.Lens[S, Long @@ Generation]): Termination[S] = State { state: S => (state, step.get(state) >= max) }

  def afterTime[S](max: Duration)(implicit time: monocle.Lens[S, Long @@ Start]): Termination[S] = State {
    state: S =>
      val duration = (System.currentTimeMillis - time.get(state)).millis
      (state, duration >= max)
  }

  def never[S] = State { state: S => (state, false) }

  def dynamicOperator[G, P, OP](pop: Population[Individual[G, P]], genomePart: monocle.Lens[G, Option[Int]], exploration: Double, ops: Vector[OP]): State[Random, OP] = State { rng: Random =>
    def stats(p: Population[Individual[G, P]]) = {
      val working = p.flatMap(i => genomePart.get(i.genome))
      val count = working.groupBy(identity)
      (0 until ops.size).map {
        i =>
          val size = count.getOrElse(i, Nil).size
          i -> (size.toDouble / working.size)
      }
    }

    def selected: Int =
      if (rng.nextDouble < exploration) rng.nextInt(ops.size)
      else multinomial(stats(pop).toList)(rng)

    (rng, ops(selected))
  }

}

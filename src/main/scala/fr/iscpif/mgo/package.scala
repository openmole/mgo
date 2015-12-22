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

import scala.util.control.TailCalls._

package object mgo {

  /**** Running the EA ****/

  def stepEA[M[_]: Monad, I, G](
    preStep: Vector[I] => M[Unit],
    breeding: Breeding[M, I, G],
    expression: Expression[G, I],
    objective: Objective[M, I],
    replacementStrategy: (Vector[I], Vector[I]) => Vector[I]): Kleisli[M, Vector[I], Vector[I]] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      _ <- Kleisli.kleisli[M, Vector[I], Unit](preStep)
      bred <- breeding
      expressed = bred.par.map(expression).toVector
      kept <- thenK(objective)(replacementStrategy(population, expressed))
    } yield kept

  def runEA[M[_]: Monad, I](stepFunction: Kleisli[M, Vector[I], Vector[I]]): Kleisli[M, Vector[I], Vector[I]] =
    runEAUntil[M, I](
      Kleisli.kleisli[M, Vector[I], Boolean]({ (_: Vector[I]) => false.point[M] }),
      stepFunction)

  /*def runEAUntilNR[ M[_]: Monad, , IG](
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

  /** Non-tail recursive. Will break for long runs. */
  def runEAUntil[M[_]: Monad, I](
    stopCondition: Kleisli[M, Vector[I], Boolean],
    stepFunction: Kleisli[M, Vector[I], Vector[I]]): Kleisli[M, Vector[I], Vector[I]] =
    for {
      population <- Kleisli.ask[M, Vector[I]]
      stop <- stopCondition
      res <- if (stop) Kleisli.ask[M, Vector[I]]
      else stepFunction >=> runEAUntil[M, I](stopCondition, stepFunction)
    } yield res

  /*def runEAUntilStackless[M[_]: Traverse: Monad, I](
    stopCondition: Kleisli[M, Vector[I], Boolean],
    stepFunction: Kleisli[M, Vector[I], Vector[I]])(population: Vector[I]): Free.Trampoline[M[Vector[I]]] =
    Traverse.apply[M].sequence[Free.Trampoline, Vector[I]](for {
      stop <- stopCondition.run(population)
      newpop <- if (stop) population.point[M] else stepFunction(population)
    } yield {
      if (stop) Free.return_[Function0, M[Vector[I]]](newpop.point[M])
      else Free.suspend[Function0, M[Vector[I]]](runEAUntilStackless(stopCondition, stepFunction)(newpop))
    })*/

  /*def runEAUntilStackless[S, I](
    stopCondition: Kleisli[({type l[x]=State[S,x]})#l, Vector[I], Boolean],
    stepFunction: Kleisli[({type l[x]=State[S,x]})#l, Vector[I], Vector[I]])(population: Vector[I]): Kleisli[({type l[x]=State[S,x]})#l, Vector[I], Vector[I]] = {
    val (s, stop) = stopCondition.run(population).run()
  }*/

  /** ** Stop conditions ****/

  def anyReaches[M[_]: Monad, I](goalReached: I => Boolean)(population: Vector[I]): Vector[I] => M[Boolean] =
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

  def bindB[M[_]: Monad, I, G1, G2](b1: Breeding[M, I, G1], b2: Vector[G1] => Breeding[M, I, G2]): Breeding[M, I, G2] =
    Breeding((individuals: Vector[I]) => for {
      g1s <- b1(individuals)
      g2s <- b2(g1s)(individuals)
    } yield g2s)

  def zipB[M[_]: Monad, I, G1, G2](b1: Breeding[M, I, G1], b2: Breeding[M, I, G2]): Breeding[M, I, (G1, G2)] = zipWithB { (g1: G1, g2: G2) => (g1, g2) }(b1, b2)

  def zipWithB[M[_]: Monad, I, G1, G2, G3](f: ((G1, G2) => G3))(b1: Breeding[M, I, G1], b2: Breeding[M, I, G2]): Breeding[M, I, G3] =
    Breeding((individuals: Vector[I]) =>
      for {
        g1s <- b1(individuals)
        g2s <- b2(individuals)
      } yield (g1s, g2s).zipped.map(f))

  def productB[M[_]: Monad, I, G1, G2](b1: Breeding[M, I, G1], b2: G1 => Breeding[M, I, G2]): Breeding[M, I, G2] =
    productWithB[M, I, G1, G2, G2] { (_: G1, g2: G2) => g2 }(b1, b2)

  def asB[M[_]: Monad, I, I1, G1, G](itoi1: I => I1, g1tog: G1 => G, breeding: Breeding[M, I1, G1]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) =>
      breeding(individuals.map(itoi1)).map[Vector[G]] { (g1s: Vector[G1]) => g1s.map(g1tog) })

  def productWithB[M[_]: Monad, I, G1, G2, G3](f: (G1, G2) => G3)(b1: Breeding[M, I, G1], b2: G1 => Breeding[M, I, G2]): Breeding[M, I, G3] =
    Breeding((individuals: Vector[I]) =>
      for {
        g1s <- b1(individuals)
        nested <- g1s.traverse[M, Vector[G3]] { (g1: G1) => b2(g1)(individuals).map { (g2s: Vector[G2]) => g2s.map { (g2: G2) => f(g1, g2) } } }
      } yield Monad[Vector].join(nested))

  def mapB[M[_]: Monad, I, G](op: I => M[G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.traverse[M, G](op))

  def mapPureB[M[_]: Monad, I, G](op: I => G): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.traverse[M, G](op(_: I).point[M]))

  def flatMapB[M[_]: Monad, I, G](op: I => M[Vector[G]]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.traverseM[M, G](op))

  def byNicheB[I, N, M[_]: Monad, G](niche: I => N)(breeding: Breeding[M, I, G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => {
      val indivsByNiche: Map[N, Vector[I]] = individuals.groupBy(niche)
      indivsByNiche.valuesIterator.toVector.traverse[M, Vector[G]](breeding).map[Vector[G]](_.flatten)
    })

  def flatMapPureB[M[_]: Monad, I, G](op: I => Vector[G]): Breeding[M, I, G] =
    Breeding((individuals: Vector[I]) => individuals.traverseM[M, G](op(_: I).point[M]))

  def probabilisticOperatorB[M[_]: Monad: RandomGen, I, G](
    opsAndWeights: Vector[(Kleisli[M, I, G], Double)]): Kleisli[M, I, (G, Int)] =
    Kleisli((mates: I) => {
      for {
        rg <- implicitly[RandomGen[M]].split
        op = multinomial[Int](opsAndWeights.zipWithIndex.map { case ((op, w), i) => (i, w) }.toList)(rg)
        g <- opsAndWeights(op)._1.run(mates)
      } yield (g, op)
    })

  /** Breed a genome for subsequent stochastic expression */
  def withRandomGenB[M[_]: Monad: RandomGen, I]: Breeding[M, I, (Random, I)] =
    Breeding((individuals: Vector[I]) =>
      for {
        rgs <- implicitly[RandomGen[M]].split.replicateM(individuals.size)
      } yield rgs.toVector zip individuals)

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

  def bindO[M[_]: Monad, I](o1: Objective[M, I], o2: Vector[I] => Objective[M, I]): Objective[M, I] =
    Objective((phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(selected1s)(phenotypes)
      } yield selected2s)

  def andO[M[_]: Monad, I](o1: Objective[M, I], o2: Objective[M, I]): Objective[M, I] =
    Objective((phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(phenotypes)
      } yield selected1s.intersect(selected2s))

  def orO[M[_]: Monad, I](o1: Objective[M, I], o2: Objective[M, I]): Objective[M, I] =
    Objective((phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(phenotypes)
      } yield selected1s.union(selected2s))

  def thenO[M[_]: Monad, I](o1: Objective[M, I], o2: Objective[M, I]): Objective[M, I] =
    Objective((phenotypes: Vector[I]) =>
      for {
        selected1s <- o1(phenotypes)
        selected2s <- o2(selected1s)
      } yield selected2s)

  def byNicheO[M[_]: Monad, I, N](niche: I => N, objective: Objective[M, I]): Objective[M, I] =
    Objective((individuals: Vector[I]) => {
      val indivsByNiche: Map[N, Vector[I]] = individuals.groupBy(niche)
      indivsByNiche.valuesIterator.toVector.traverse[M, Vector[I]](objective).map[Vector[I]](_.flatten)
    })

  /**** Helper functions ****/

  /**
   * This function helps chaining Kleisli arrows in for comprehensions. For example, given two Kleisli arrows
   * k of type Kleisli[M,A,B] and l of type Kleisli[M,B,C], the following are equivalent:
   *
   * k >=> l
   *
   * for {
   *   a <- k
   *   b <- thenK(l)(a)
   * } yield b
   */
  def thenK[M[_]: Monad, R, A, B](k: Kleisli[M, A, B])(a: A): Kleisli[M, R, B] =
    Kleisli.kleisli[M, R, B](_ => k.run(a))

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

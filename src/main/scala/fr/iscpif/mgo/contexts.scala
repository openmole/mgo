/*
 * Copyright (C) 07/12/2015 Guillaume Chérel
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
package fr.iscpif.mgo

import stop._
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.effect.IO

import scala.util.Random

object contexts {

  trait RandomGen[M[_]] {
    /** returns the random number generator in M */
    def random: M[Random]
  }

  trait ParallelRandomGen[M[_]] {
    /**
     * Returns a new random number generator that is independant from the one in M, useful for parallel computations.
     * Implementations of this function must use a random number generator contained in M in order to produce the Random returned, and update the original
     * random number generator in an independant manner so that it is never used twice (and allows for safe parallelisation).
     */
    def split: M[Random]
  }

  trait Generational[M[_]] {
    def getGeneration: M[Long]
    def setGeneration(i: Long): M[Unit]
    def incrementGeneration: M[Unit]
  }

  trait StartTime[M[_]] {
    def startTime: M[Long]
  }

  trait HitMapper[M[_], C] {
    def get: M[Map[C, Int]]
    def set(newMap: Map[Vector[Int], Int]): M[Unit]
    def hitCount(cell: C): M[Int]
    def addHit(cell: C): M[Unit]
    def addHits(cells: Vector[C]): M[Unit]
    def removeHit(cell: C): M[Unit]
  }

  object default {

    case class EvolutionData[S](
      generation: Long = 0,
      startTime: Long = System.currentTimeMillis(),
      random: Random = newRNG(System.currentTimeMillis()),
      s: S)

    type EvolutionState[S, T] = StateT[IO, EvolutionData[S], T]

    def wrap[S, T](x: (EvolutionData[S], T)): EvolutionState[S, T] = StateT.apply[IO, EvolutionData[S], T](_ => IO(x))
    def unwrap[S, T](s: S)(x: EvolutionState[S, T]): (EvolutionData[S], T) = x(EvolutionData[S](0, 0, Random, s)).unsafePerformIO

    implicit def evolutionStateMonad[S]: Monad[EvolutionState[S, ?]] = StateT.stateTMonadState[EvolutionData[S], IO]
    implicit def evolutionStateMonadState[S]: MonadState[EvolutionState[S, ?], EvolutionData[S]] = StateT.stateTMonadState[EvolutionData[S], IO]
    implicit def evolutionStateMonadTrans[S]: MonadTrans[StateT[?[_], EvolutionData[S], ?]] = StateT.StateMonadTrans[EvolutionData[S]]

    implicit def evolutionStateUseRG[S]: RandomGen[EvolutionState[S, ?]] = new RandomGen[EvolutionState[S, ?]] {
      def random: EvolutionState[S, Random] =
        for {
          s <- evolutionStateMonadState[S].get
        } yield s.random
    }

    implicit def evolutionStateUseParallelRG[S]: ParallelRandomGen[EvolutionState[S, ?]] = new ParallelRandomGen[EvolutionState[S, ?]] {
      def split: EvolutionState[S, Random] =
        for {
          s <- evolutionStateMonadState[S].get
          rg = s.random
          rg1 = newRNG(rg.nextLong())
        } yield rg1
    }

    implicit def evolutionStateGenerational[S]: Generational[EvolutionState[S, ?]] = new Generational[EvolutionState[S, ?]] {
      def getGeneration: EvolutionState[S, Long] = evolutionStateMonadState[S].get.map(_.generation)

      def setGeneration(i: Long): EvolutionState[S, Unit] =
        for {
          s <- evolutionStateMonadState[S].get
          _ <- evolutionStateMonadState[S].put(s.copy(generation = i))
        } yield ()

      def incrementGeneration: EvolutionState[S, Unit] =
        getGeneration >>= { generation => setGeneration(generation + 1) }

      def generationReached(x: Long): EvolutionState[S, Boolean] =
        for {
          g <- getGeneration
        } yield g >= x
    }

    implicit def evolutionStartTime[S]: StartTime[EvolutionState[S, ?]] = new StartTime[EvolutionState[S, ?]] {
      def startTime: EvolutionState[S, Long] = evolutionStateMonadState[S].get.map(_.startTime)
    }

    def liftIOValue[S, A](mio: EvolutionState[S, IO[A]]): EvolutionState[S, A] =
      for {
        ioa <- mio
        a <- evolutionStateMonadTrans[S].liftM[IO, A](ioa)
      } yield a

    def writeS[S, I](writeFun: (EvolutionData[S], Vector[I]) => String, ioFun: String => IO[Unit] = IO.putStrLn): Kleisli[EvolutionState[S, ?], Vector[I], Unit] =
      Kleisli.kleisli[EvolutionState[S, ?], Vector[I], Unit] { (is: Vector[I]) =>
        for {
          s <- evolutionStateMonadState[S].get
          io <- ioFun(writeFun(s, is)).point[EvolutionState[S, ?]]
          _ <- evolutionStateMonadTrans[S].liftM[IO, Unit](io)
        } yield ()
      }

    def runEAUntilStackless[S, I](
      stopCondition: StopCondition[EvolutionState[S, ?], I],
      stepFunction: Kleisli[EvolutionState[S, ?], Vector[I], Vector[I]],
      start: EvolutionData[S]): Kleisli[EvolutionState[S, ?], Vector[I], Vector[I]] = {

      @tailrec
      def tailRec(start: EvolutionData[S], population: Vector[I]): EvolutionState[S, Vector[I]] = {
        val (s1, stop) = stopCondition.run(population).run(start).unsafePerformIO
        if (stop) StateT.apply[IO, EvolutionData[S], Vector[I]] { _ => IO((s1, population)) }
        else {
          val (s2, newpop) = stepFunction.run(population).run(s1).unsafePerformIO
          tailRec(s2, newpop)
        }
      }

      Kleisli.kleisli[EvolutionState[S, ?], Vector[I], Vector[I]] { population: Vector[I] => tailRec(start, population) }
    }

  }
}


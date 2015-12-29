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

import fr.iscpif.mgo.Breedings._
import fr.iscpif.mgo.Objectives._

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.effect.IO

import scala.util.Random

object Contexts {

  trait RandomGen[M[_]] {
    /** returns the random number generator in M */
    def get: M[Random]
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
    def generationReached(x: Long): M[Boolean]
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
    type EvolutionStateMonad[S] = ({ type l[x] = EvolutionState[S, x] })

    def wrap[S, T](x: (EvolutionData[S], T)): EvolutionState[S, T] = StateT.apply[IO, EvolutionData[S], T](_ => IO(x))
    def unwrap[S, T](s: S)(x: EvolutionState[S, T]): (EvolutionData[S], T) = x(EvolutionData[S](0, 0, Random, s)).unsafePerformIO

    implicit def evolutionStateMonad[S]: Monad[EvolutionStateMonad[S]#l] = StateT.stateTMonadState[EvolutionData[S], IO]
    implicit def evolutionStateMonadState[S]: MonadState[EvolutionStateMonad[S]#l, EvolutionData[S]] = StateT.stateTMonadState[EvolutionData[S], IO]
    implicit def evolutionStateMonadTrans[S]: MonadTrans[({ type L[f[_], a] = StateT[f, EvolutionData[S], a] })#L] = StateT.StateMonadTrans[EvolutionData[S]]

    implicit def evolutionStateUseRG[S]: RandomGen[EvolutionStateMonad[S]#l] = new RandomGen[EvolutionStateMonad[S]#l] {
      def get: EvolutionState[S, Random] =
        for {
          s <- evolutionStateMonadState[S].get
        } yield s.random

      def split: EvolutionState[S, Random] =
        for {
          s <- evolutionStateMonadState[S].get
          rg = s.random
          //TODO: est-ce que c'est une bonne manière de générer 2 nouveaux générateurs aléatoires indépendants?
          rg1 = newRNG(rg.nextLong())
          rg2 = newRNG(rg.nextLong())
          _ <- evolutionStateMonadState[S].put(s.copy(random = rg2))
        } yield rg1
    }

    implicit def evolutionStateGenerational[S]: Generational[EvolutionStateMonad[S]#l] = new Generational[EvolutionStateMonad[S]#l] {
      def getGeneration: EvolutionState[S, Long] =
        for {
          s <- evolutionStateMonadState[S].get
        } yield s.generation

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

    /*def addIOBefore[S, A, B](mio: EvolutionState[S, IO[A]], action: A => EvolutionState[S, B]): EvolutionState[S, B] =
      for {
        ioa <- mio
        a <- evolutionStateMonadTrans[S].liftM[IO, A](ioa)
        res <- action(a)
      } yield res*/

    def liftIOValue[S, A](mio: EvolutionState[S, IO[A]]): EvolutionState[S, A] =
      for {
        ioa <- mio
        a <- evolutionStateMonadTrans[S].liftM[IO, A](ioa)
      } yield a

    def writeS[S, I](writeFun: (EvolutionData[S], Vector[I]) => String, ioFun: String => IO[Unit] = IO.putStrLn): Kleisli[EvolutionStateMonad[S]#l, Vector[I], Unit] =
      Kleisli.kleisli[EvolutionStateMonad[S]#l, Vector[I], Unit] { (is: Vector[I]) =>
        for {
          s <- evolutionStateMonadState[S].get
          io <- ioFun(writeFun(s, is)).point[EvolutionStateMonad[S]#l]
          _ <- evolutionStateMonadTrans[S].liftM[IO, Unit](io)
        } yield ()
      }

    def runEAUntilStackless[S, I](
      stopCondition: Kleisli[EvolutionStateMonad[S]#l, Vector[I], Boolean],
      stepFunction: Kleisli[EvolutionStateMonad[S]#l, Vector[I], Vector[I]],
      start: EvolutionData[S]): Kleisli[EvolutionStateMonad[S]#l, Vector[I], Vector[I]] = {

      @tailrec
      def tailRec(start: EvolutionData[S], population: Vector[I]): EvolutionState[S, Vector[I]] = {
        val (s1, stop) = stopCondition.run(population).run(start).unsafePerformIO
        if (stop) StateT.apply[IO, EvolutionData[S], Vector[I]] { _ => IO((s1, population)) }
        else {
          val (s2, newpop) = stepFunction.run(population).run(s1).unsafePerformIO
          tailRec(s2, newpop)
        }
      }

      Kleisli.kleisli[EvolutionStateMonad[S]#l, Vector[I], Vector[I]] { population: Vector[I] => tailRec(start, population) }
    }
  }
}


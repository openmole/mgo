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

  object default {

    case class EvolutionData[S](
      generation: Long = 0,
      startTime: Long @@ Start = Tag.of[Start](System.currentTimeMillis()),
      random: Random = newRNG(System.currentTimeMillis()),
      s: S)

    type EvolutionState[S, T] = StateT[IO, EvolutionData[S], T]
    type EvolutionStateMonad[S] = ({ type l[x] = EvolutionState[S, x] })

    def wrap[S, T](x: (EvolutionData[S], T)): EvolutionState[S, T] = StateT.apply[IO, EvolutionData[S], T](_ => IO(x))
    def unwrap[S, T](s: S)(x: EvolutionState[S, T]): (EvolutionData[S], T) = x(EvolutionData[S](0, Tag.of[Start](0), Random, s)).unsafePerformIO

    implicit def evolutionStateUseRG[S]: RandomGen[EvolutionStateMonad[S]#l] = new RandomGen[EvolutionStateMonad[S]#l] {
      def get: EvolutionState[S, Random] =
        for {
          s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].get
        } yield s.random

      def split: EvolutionState[S, Random] =
        for {
          s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].get
          rg = s.random
          //TODO: est-ce que c'est une bonne manière de générer 2 nouveaux générateurs aléatoires indépendants?
          rg1 = newRNG(rg.nextLong())
          rg2 = newRNG(rg.nextLong())
          _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].put(s.copy(random = rg2))
        } yield rg1
    }

    implicit def evolutionStateGenerational[S]: Generational[EvolutionStateMonad[S]#l] = new Generational[EvolutionStateMonad[S]#l] {
      def getGeneration: EvolutionState[S, Long] =
        for {
          s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].get
        } yield s.generation

      def setGeneration(i: Long): EvolutionState[S, Unit] =
        for {
          s <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].get
          _ <- implicitly[MonadState[({ type T[s, a] = StateT[IO, s, a] })#T, EvolutionData[S]]].put(s.copy(generation = i))
        } yield ()

      def incrementGeneration: EvolutionState[S, Unit] =
        getGeneration >>= { (generation: Long) => setGeneration(generation + 1) }

      def generationReached(x: Long): EvolutionState[S, Boolean] =
        for {
          g <- getGeneration
        } yield g >= x
    }

    def addIOBefore[S, A, B](mio: EvolutionState[S, IO[A]], action: A => EvolutionState[S, B]): EvolutionState[S, B] =
      for {
        ioa <- mio
        a <- implicitly[MonadTrans[({ type L[f[_], a] = StateT[f, EvolutionData[S], a] })#L]].liftM[IO, A](ioa)
        res <- action(a)
      } yield res

    def liftIOValue[S, A](mio: EvolutionState[S, IO[A]]): EvolutionState[S, A] =
      for {
        ioa <- mio
        a <- implicitly[MonadTrans[({ type L[f[_], a] = StateT[f, EvolutionData[S], a] })#L]].liftM[IO, A](ioa)
      } yield a

  }
}


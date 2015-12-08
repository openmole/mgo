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

import scala.util.Random


object Contexts {

  trait UseRG[M[_]] {
    /** implementations of this function must use a random number generator contained in M in order to produce the Random returned, and update the original
      * random number generator in an independant manner so that it is never used twice (and allows for safe parallelisation). */
    def useRG: M[Random]
  }

  trait Generational[M[_]] {
    def getGeneration: M[Long]
    def setGeneration(i: Long): M[Unit]
    def incrementGeneration: M[Unit]
  }

  object default {

    case class EvolutionData(
      generation: Long = 0,
      startTime: Long @@ Start = System.currentTimeMillis(),
      random: Random = newRNG(System.currentTimeMillis()))

    type EvolutionState[S, T] = State[(EvolutionData, S), T]
    type EvolutionStateMonad[S] = ({type l[x] = EvolutionState[S, x]})

    implicit def evolutionStateUseRG[S]: UseRG[EvolutionStateMonad[S]#l] = new UseRG[EvolutionStateMonad[S]#l] {

      def useRG: EvolutionState[S, Random] =
        for {
          s <- State.get[(EvolutionData, S)]
          rg = s._1.random
          //TODO: est-ce que c'est une bonne manière de générer 2 nouveaux générateurs aléatoires indépendants?
          rg1 = newRNG(rg.nextLong())
          rg2 = newRNG(rg.nextLong())
          _ <- State.put[(EvolutionData, S)]((s._1.copy(random = rg2), s._2))
        } yield rg1
    }

    implicit def evolutionStateGenerational[S]: Generational[({type l[x] = EvolutionState[S, x]})#l] = new Generational[({type l[x] = EvolutionState[S, x]})#l] {
      def getGeneration: EvolutionState[S, Long] =
        for {
          s <- State.get[(EvolutionData, S)]
        } yield s._1.generation

      def setGeneration(i: Long): EvolutionState[S, Unit] =
        for {
          s <- State.get[(EvolutionData, S)]
          _ <- State.put[(EvolutionData, S)]((s._1.copy(generation = i), s._2))
        } yield ()

      def incrementGeneration: EvolutionState[S, Unit] =
        getGeneration >>= { (generation: Long) => setGeneration(generation + 1) }
    }

  }
}


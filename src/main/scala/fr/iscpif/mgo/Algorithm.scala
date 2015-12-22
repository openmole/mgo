/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

import monocle.macros._

import scala.util.Random

import scala.language.higherKinds
import scalaz._
import Scalaz._

import Breedings._
import Expressions._
import Objectives._

/**
 * Represent a state of the evolution algorithm
 */
@Lenses case class CommonState(
  generation: Long @@ Generation,
  startTime: Long @@ Start = System.currentTimeMillis(),
  random: Random)

case class AlgorithmState[S](
  common: CommonState,
  state: S)

trait AlgorithmOld[G, P, S] {

  type Ind = Individual[G, P]
  type Pop = Population[Ind]

  def initialState: S

  def algorithmState(random: Random, generation: Long = 0) =
    AlgorithmState(state = initialState, common = CommonState(random = random, generation = generation))

  def breeding(population: Pop, lambda: Int): State[AlgorithmState[S], Vector[G]]
  def elitism(population: Pop, offspring: Pop): State[AlgorithmState[S], Pop]

}

/**
 * Example:
 * Let type C[A] = (SomeState,A)
 *
 * // Initialisation
 * val (initialState, initialGs) = unwrap(initialGenomes)
 * val initialPop = initialGs.map(express)
 *
 * // First step:
 * val (s11, genomes1) = run((initialState,initialPop), breeding)
 * val indivs1 = genomes1.map(express)
 * val (s12, selected1) = run((s11,indivs1), elitism)
 *
 * // Second step:
 * val (s21, genomes2) = run((s12, selected1), breeding)
 * val indivs2 = genomes2.map(express)
 * val (s22, selected2) = run((s21, indivs2), elitism)
 */
trait Algorithm[M[_], I, G, C[_]] {

  implicit val m: Monad[M]

  def initialGenomes: M[Vector[G]]
  def breeding: Breeding[M, I, G]
  def expression: Expression[G, I]
  def elitism: Objective[M, I]
  def step: Kleisli[M, Vector[I], Vector[I]]
  /** Turn a non monadic value into a monadic one. */
  def wrap[A](m: C[A]): M[A]
  def unwrap[A](m: M[A]): C[A]

  def run[A, B](start: C[A], action: A => M[B]): C[B] =
    unwrap(
      for {
        a <- wrap(start)
        b <- action(a)
      } yield b
    )
}

/*trait AlgorithmOpenMOLE[M[_], I, G, C] {

  implicit val m: Monad[M]

  val cRandom: Lens[C, Random]
  val iAge: Lens[I, Long]

  def initialGenomes(n: Int): M[Vector[G]]
  def breeding(n: Int): Breeding[M, I, G]
  def elitism: Objective[M, I]

  def initForIsland(i: I): I

  def wrap[A](m: (C, A)): M[A]
  def unwrap[A](m: M[A]): (C, A)

  def run[A, B](start: (C, A), action: A => M[B]): (C, B) =
    unwrap(
      for {
        a <- wrap(start)
        b <- action(a)
      } yield b
    )
}*/

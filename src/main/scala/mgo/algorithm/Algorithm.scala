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
package mgo.algorithm

import mgo._

import scala.language.higherKinds
import scala.util.Random
import cats.data._

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
trait Algorithm[T, M[_], I, G, S] {
  //def initialState(t: T, rng: Random): S
  def initialPopulation(t: T): M[Vector[I]]
  def step(t: T): Kleisli[M, Vector[I], Vector[I]]
  def state: M[S]
  // def run[A](m: M[A], s: S): A
}


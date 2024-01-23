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
package mgo.tools.execution

import scala.language.higherKinds
import scala.util.Random
import cats.data.*
import org.apache.commons.math3.random.RandomAdaptor

import scala.concurrent.ExecutionContext

object Algorithm:
  lazy val parallel = Parallel(ExecutionContext.global, s => util.Random(s))

  sealed trait ParallelContext
  case class Parallel(executionContext: ExecutionContext, seeder: Long => Random) extends ParallelContext
  object Sequential extends ParallelContext

trait Algorithm[T, I, G, S]:
  def initialState(t: T, rng: Random): S
  def initialPopulation(t: T, rng: Random, parallel: Algorithm.ParallelContext): Vector[I]
  def step(t: T): (S, Vector[I], Random, Algorithm.ParallelContext) => (S, Vector[I])


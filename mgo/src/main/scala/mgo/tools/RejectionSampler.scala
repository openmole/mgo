package mgo.tools

/*
 * Copyright (C) 2021 Romain Reuillon
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import java.util.Random

object RejectionSampler:

  case class State(test: Long = 0L, pass: Long = 0L)

  def success(state: State) = State(state.test + 1, state.pass + 1)
  def fail(state: State) = State(state.test + 1, state.pass)
  def allFailed(state: State) = state.pass == 0L

  def warmup(sampler: RejectionSampler, n: Int, state: State = State()): State =
    if n > 0
    then
      val (x, _) = sampler.sampleFunction()
      if !sampler.accept(x)
      then warmup(sampler, n - 1, RejectionSampler.fail(state))
      else warmup(sampler, n - 1, RejectionSampler.success(state))
    else state

  def sample(sampler: RejectionSampler, state: State = State()): (State, (IArray[Double], Double)) =
    val (x, density) = sampler.sampleFunction()
    if !sampler.accept(x)
    then sample(sampler, RejectionSampler.fail(state))
    else
      val newState = RejectionSampler.success(state)
      val inverseProbability = newState.test.toDouble / newState.pass
      (newState, (x, density.value / inverseProbability))

  def sampleArray(sampler: RejectionSampler, n: Int, state: State = State(), res: List[(IArray[Double], Double)] = List()): (State, IArray[(IArray[Double], Double)]) =
    if n > 0
    then
      val (newState, newSample) = sample(sampler, state)
      sampleArray(sampler, n - 1, newState, newSample :: res)
    else (state, IArray.unsafeFromArray(res.reverse.toArray))

case class RejectionSampler(
  sampleFunction: () => (IArray[Double], Lazy[Double]),
  accept: IArray[Double] => Boolean)


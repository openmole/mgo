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
      val x = sampler.sample()
      if !sampler.accept(x)
      then warmup(sampler, n - 1, RejectionSampler.fail(state))
      else warmup(sampler, n - 1, RejectionSampler.success(state))
    else state

  def sampleNoDensity(sampler: RejectionSampler): IArray[Double] =
    val x = sampler.sample()
    if !sampler.accept(x)
    then sampleNoDensity(sampler)
    else x

  def sample(sampler: RejectionSampler, state: State = State()): (State, IArray[Double]) =
    val x = sampler.sample()
    if !sampler.accept(x)
    then sample(sampler, RejectionSampler.fail(state))
    else
      val newState = RejectionSampler.success(state)
      (newState, x)

  def density(state: State, density: Double) =
    val inverseProbability = state.test.toDouble / state.pass
    density / inverseProbability


case class RejectionSampler(
  sample: () => IArray[Double],
  accept: IArray[Double] => Boolean)


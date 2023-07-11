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

object RejectionSampler {

  /**
   * Monte Carlo estimation of the success rate of the predicate.
   *
   * @param test
   * @param pass
   */
  case class State(test: Long = 0L, pass: Long = 0L) {
    def inverseProbability() = test.toDouble / pass
  }

  def success(state: State) = State(state.test + 1, state.pass + 1)
  def fail(state: State) = State(state.test + 1, state.pass)
  def noSuccess(state: State) = state.pass == 0

}

import RejectionSampler._
import scala.annotation.tailrec
/**
 * Rejection sampler with a predicate and a state.
 *
 * @param dist
 * @param patternFunction
 * @param accept
 */
class RejectionSampler(_sample: () => (Vector[Double], Lazy[Double]), val accept: Vector[Double] => Boolean) {

  def warmup(n: Int, state: State = State()): State =
    if (n > 0) {
      val (x, _) = _sample()
      if (!accept(x)) warmup(n - 1, fail(state))
      else warmup(n - 1, success(state))
    } else state

  def sample(state: State = State()): (State, (Vector[Double], Double)) = {
    @tailrec def sample0(state: State): (State, (Vector[Double], Double)) = {
      val (x, density) = _sample()
      if (!accept(x)) {
        // if the sample is rejected, resample and keep the failure in the state
        sample0(fail(state))
      } else {
        val newState = success(state)
        // if the sample is accepted, return the state, the sample pattern and the adjusted density
        (newState, (x, density.value / newState.inverseProbability()))
      }
    }

    sample0(state)
  }

  @tailrec final def sampleVector(n: Int, state: State = State(), res: List[(Vector[Double], Double)] = List()): (State, Vector[(Vector[Double], Double)]) = {
    if (n > 0) {
      val (newState, newSample) = sample(state)
      sampleVector(n - 1, newState, newSample :: res)
    } else (state, res.reverse.toVector)
  }

}


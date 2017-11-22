/*
 * Copyright (C) 10/07/2017 Guillaume Chérel
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
package mgo.algorithm.sensitivityAnalysis

/** Various sensitivity analisys methods. */
package object sensitivityAnalysis {

  /**
   * Total order effect sensitivity analysis.
   *
   * With `s` an instance of this case class, `s.run(f)` performs the
   * sensitivity analysis of model `f` and returns it as a value of type
   * `S`. The return value must then be passed to the function sT such
   * that `s.run(i, s.run(f))` gives the sensitivity index for the model
   * i-th parameter.
   *
   * Instances of this case class are used to compute the total order
   * effect of each model parameter on the model output, i.e. first
   * order effect and interactions. They respect the following equation:
   *
   * s.sT(i, s.run(f)) ≃ 
   *   E_{X_{~i}}[Var_{X_i}(f(X_1, ..., X_k) | X_{~i})]
   */
  case class SATotalOrder[X, Y](run: (X => Y) => Vector[Double])

}

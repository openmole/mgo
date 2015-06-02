/*
 * Copyright (C) 2015 Guillaume Chérel
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

package fr.iscpif.mgo.tools.network

trait NeuralNetwork {
  def activationF(x: Double): Double

  def feedForwardOnce(inputValues: Seq[Double]): Seq[Double] = ???

  def feedForwardUntilStable(inputValues: Seq[Double]) = ???
}

object NeuralNetwork {
  def apply(
    inputnodes: Seq[Int],
    outputnodes: Seq[Int],
    bias: Boolean,
    edges: Seq[(Int, Int, Double)],
    activationfunction: Double => Double): NeuralNetwork = ???
}

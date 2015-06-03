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

package fr.iscpif.mgo.tools.neuralnetwork

import fr.iscpif.mgo.tools.network._
import math._

/**
 * @tparam N the neurons output type
 * @tparam E the weights type
 */
trait NeuralNetwork[N, W] {
  def network: Network[Unit, W] with DirectedEdges[W]
  def inputNeurons: Vector[Int]
  def outputNeurons: Vector[Int]
}

trait Feedforward[N] {
  def query(inputValues: Seq[N]): Seq[N] = ???
}

trait Recurrent[N] {
  /**
   * Activating the network means computing the output values for all neurons simultaneously.
   * There is no order effect. The activation is done steps times. For the first activation, the
   * input neurones are initialised to the inputValues, and the other neurones to initialNeuronState
   */
  def activate(steps: Int, inputValues: Seq[N]): Seq[N] = ???
  def activateUntilStable(maxsteps: Int, inputValues: Seq[N]): Seq[N] = ???

  /**
   * To propagate the inputValues means initialising the input neurons to the input values,
   * then activating the neurons connected to the input, and successively activating the rest of the network. Steps determines
   * the number of successive steps taken before returning the values of the output neurons.
   */
  def propagate(steps: Int, inputValues: Seq[N]): Seq[N] = ???
  def propagateUntilStable(maxsteps: Int, inputValues: Seq[N]): Seq[N] = ???

  def initialNeuronState: N
}

trait HomogeneousActivationFunction[N, W] {
  def activate(neuron: Int): N = activationFunction(inputs(neuron))

  def activationFunction(inputsAndWeights: Traversable[(N, W)]): N
  def inputs(neuron: Int): Vector[(N, W)]
}

trait HeterogeneousActivationFunction[N, W] {
  def activate(neuron: Int): N = activationFunction(neuron)(inputs(neuron))

  def activationFunction: IndexedSeq[Traversable[(N, W)] => N]
  def inputs(neuron: Int): Vector[(N, W)]
}

// trait CPPN <: NeuralNetwork[Double] with HeterogeneousActivationFunction[Double] {
// }

object NeuralNetwork {
  def feedforwardNetwork[N, W](
    inputnodes: Seq[Int],
    outputnodes: Seq[Int],
    bias: Boolean,
    edges: Seq[(Int, Int, Double)],
    activationfunction: Traversable[(N, W)] => N): NeuralNetwork[N, W] with Feedforward[N] = ???
}

object ActivationFunction {
  def tanh(inputsAndWeights: Traversable[(Double, Double)]): Double = math.tanh(weightedSum(inputsAndWeights))
  def logistic(inputsAndWeights: Traversable[(Double, Double)]): Double = 1.0 / (1 + math.exp(-weightedSum(inputsAndWeights)))

  def weightedSum(inputsAndWeights: Traversable[(Double, Double)]): Double =
    inputsAndWeights.foldLeft(0.0) { case (sum, (input, weight)) => sum + (input * weight) }

}

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
import scala.annotation.tailrec

/**
 * @tparam N the neurons output type
 * @tparam E the weights type
 */
trait NeuralNetwork[N, W] {
  def network: Network[Unit, W] with DirectedEdges[W]
  def inputNeurons: IndexedSeq[Int]
  def outputNeurons: IndexedSeq[Int]
  def inputsAndWeights(neuron: Int, state: IndexedSeq[N]): Vector[(N, W)] =
    network.in(neuron).map { case (innode, weight) => (state(innode), weight) }

  def stateWithInputs(inputValues: Seq[N]): IndexedSeq[N] = {
    val mutableState = state.toBuffer
    (inputValues zip inputNeurons) foreach { case (v, i) => mutableState(i) = v }
    mutableState.toIndexedSeq
  }
  def state: IndexedSeq[N]
}

trait Feedforward[N] {
  def query(inputValues: Seq[N]): Seq[N] = ???
}

trait Recurrent[N, W] {
  /**
   * Activating the network means computing the output values for all neurons simultaneously.
   * There is no order effect. The activation is done steps times. For the first activation, the
   * input neurones are initialised to the inputValues, and the other neurones to initialNeuronState
   */
  def activate(steps: Int, inputValues: Seq[N]): Seq[N] = {
    val finalState = activateRec(steps, stateWithInputs(inputValues))
    outputNeurons map { finalState(_) }
  }

  @tailrec private def activateRec(steps: Int, state: IndexedSeq[N]): IndexedSeq[N] =
    if (steps <= 0) state
    else activateRec(steps - 1, activateOnce(state))

  /** @return the number of steps, the average change over all node states during the last step, and the final state of the output neurons */
  def activateUntilStable(maxsteps: Int, stabilityThreshold: Double, inputValues: Seq[N]): (Int, Double, Seq[N]) = {
    val (steps, avgchange, finalState) = activateUntilStableRec(maxsteps, stabilityThreshold, stateWithInputs(inputValues))
    (steps, avgchange, outputNeurons.map({ finalState(_) }))
  }

  @tailrec private def activateUntilStableRec(maxsteps: Int, stabilityThreshold: Double, state: IndexedSeq[N], steps: Int = 0, avgchange: Double = 0.0): (Int, Double, IndexedSeq[N]) =
    if (steps >= maxsteps) (steps, avgchange, state)
    else {
      val nextstate = activateOnce(state)
      val avgchange = nextstate.zip(state).foldLeft[Double](0.0) { (sum, twostates) => sum + change(twostates._1, twostates._2) } / state.length
      if (avgchange < stabilityThreshold) (steps, avgchange, nextstate)
      else activateUntilStableRec(maxsteps, stabilityThreshold, nextstate, steps + 1, avgchange)
    }

  def activateOnce(state: IndexedSeq[N]): IndexedSeq[N] =
    state.zipWithIndex.map {
      case (stateval, neuron) => {
        val iaw = inputsAndWeights(neuron, state)
        if (iaw.length == 0) stateval
        else activate(neuron, iaw)
      }
    }

  /**
   * To propagate the inputValues means initialising the input neurons to the input values,
   * then activating the neurons connected to the input, and successively activating the rest of the network. Steps determines
   * the number of successive steps taken before returning the values of the output neurons.
   */
  def propagate(steps: Int, inputValues: Seq[N]): Seq[N] = ???
  def propagateUntilStable(maxsteps: Int, stabilityThreshold: Double, inputValues: Seq[N]): Seq[N] = ???

  def activate(neuron: Int, inputs: Traversable[(N, W)]): N // activate a single neuron
  def state: IndexedSeq[N]
  def inputNeurons: IndexedSeq[Int]
  def outputNeurons: IndexedSeq[Int]
  def inputsAndWeights(neuron: Int, state: IndexedSeq[N]): Vector[(N, W)]
  def stateWithInputs(inputValues: Seq[N]): IndexedSeq[N]
  def change(newstate: N, oldstate: N): Double
}

object ChangeFunction {
  def absoluteDifference(newstate: Double, oldstate: Double): Double = abs(newstate - oldstate)
}

trait HomogeneousActivationFunction[N, W] {
  def activate(neuron: Int, inputs: Traversable[(N, W)]): N = activationFunction(inputs)

  //def activationFunction(inputsAndWeights: Traversable[(N, W)]): N
  def activationFunction: Traversable[(N, W)] => N
}

trait HeterogeneousActivationFunction[N, W] {
  def activate(neuron: Int, inputs: Traversable[(N, W)]): N = activationFunction(neuron)(inputs)

  def activationFunction: IndexedSeq[Traversable[(N, W)] => N]
}

// trait CPPN <: NeuralNetwork[Double] with HeterogeneousActivationFunction[Double] {
// }

object NeuralNetwork {
  def feedforwardNetwork[N, W](
    _nodes: Int,
    _inputnodes: Int,
    _outputnodes: Int,
    _edges: Seq[(Int, Int, Double)],
    _activationfunction: Traversable[(N, W)] => N): NeuralNetwork[N, W] with Feedforward[N] with HomogeneousActivationFunction[N, W] = ???

  def recurrentNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: Traversable[(N, W)] => N,
    _change: (N, N) => Double,
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Recurrent[N, W] with HomogeneousActivationFunction[N, W] =
    new NeuralNetwork[N, W] with Recurrent[N, W] with HomogeneousActivationFunction[N, W] {
      require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
      require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
      require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
      val network = Network.directedSparse(_nodes, _edges)
      val state: IndexedSeq[N] = _state
      val inputNeurons: IndexedSeq[Int] = _inputnodes
      val outputNeurons: IndexedSeq[Int] = _outputnodes
      val activationFunction = _activationfunction
      def change(newstate: N, oldstate: N): Double = _change(newstate, oldstate)
    }

  def recurrentNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: IndexedSeq[Traversable[(N, W)] => N],
    _change: (N, N) => Double,
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Recurrent[N, W] with HeterogeneousActivationFunction[N, W] =
    new NeuralNetwork[N, W] with Recurrent[N, W] with HeterogeneousActivationFunction[N, W] {
      require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
      require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
      require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
      val network = Network.directedSparse(_nodes, _edges)
      val state: IndexedSeq[N] = _state
      val inputNeurons: IndexedSeq[Int] = _inputnodes
      val outputNeurons: IndexedSeq[Int] = _outputnodes
      val activationFunction = _activationfunction
      def change(newstate: N, oldstate: N): Double = _change(newstate, oldstate)
    }

}

object ActivationFunction {
  def zero: Traversable[(Double, Double)] => Double = _ => 0.0
  def tanh: Traversable[(Double, Double)] => Double = inputsAndWeights => math.tanh(weightedSum(inputsAndWeights))
  def logistic: Traversable[(Double, Double)] => Double = inputsAndWeights => 1.0 / (1 + math.exp(-weightedSum(inputsAndWeights)))
  def heaviside(h0: Double): Traversable[(Double, Double)] => Double =
    inputsAndWeights => {
      val x = weightedSum(inputsAndWeights)
      if (x < 0) 0
      else if (x > 0) 1
      else h0
    }

  def weightedSum(inputsAndWeights: Traversable[(Double, Double)]): Double =
    inputsAndWeights.foldLeft(0.0) { case (sum, (input, weight)) => sum + (input * weight) }

}

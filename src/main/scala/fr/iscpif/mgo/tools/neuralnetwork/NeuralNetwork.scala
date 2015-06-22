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
import java.lang.IndexOutOfBoundsException

/**
 * @tparam N the neurons output type
 * @tparam E the weights type
 */
trait NeuralNetwork[N, W] {
  def network: Network[Unit, W] with DirectedEdges[W]
  def inputNeurons: Vector[Int]
  def outputNeurons: Vector[Int]
  def inputsAndWeights(neuron: Int, state: IndexedSeq[N]): Vector[(N, W)] =
    network.in(neuron).map { case (innode, weight) => (state(innode), weight) }

  def updateState(state: Traversable[N], neuronsValues: Traversable[(Int, N)]): Vector[N] = {
    val mutableState = state.toBuffer
    neuronsValues foreach {
      case (i, v) => mutableState(i) = v
    }
    mutableState.toVector
  }

  def outNeighbours(neuron: Int) = network.outneighbours(neuron)

  def outputState(state: IndexedSeq[N]): Vector[N] = outputNeurons map { state(_) }

  def state: IndexedSeq[N]
}

trait Feedforward[N, W] {
  def query(inputValues: Seq[N]): Vector[N] =
    queryRec(updateState(state, inputNeurons zip inputValues), inputNeurons)

  @tailrec private def queryRec(state: Vector[N], currentNeurons: Vector[Int]): Vector[N] = {
    val nextNeurons = currentNeurons.toSet.flatMap { n: Int => outNeighbours(n) }.toVector

    println(currentNeurons, nextNeurons)
    //if (currentNeurons == nextNeurons) throw new RuntimeException(state.indices.map { outNeighbours(_) }.toString)
    if (nextNeurons.length == 0) state
    else {
      val nextValues = nextNeurons.map { n =>
        val iaw = inputsAndWeights(n, state)
        if (iaw.length == 0) state(n)
        else activate(n, iaw)
      }
      val nextState = updateState(state, nextNeurons zip nextValues)
      queryRec(nextState, nextNeurons)

    }
  }

  def activate(neuron: Int, inputs: Traversable[(N, W)]): N // activate a single neuron
  def state: Vector[N]
  def inputNeurons: Vector[Int]
  def inputsAndWeights(neuron: Int, state: IndexedSeq[N]): Vector[(N, W)]
  def updateState(state: Traversable[N], neuronsValues: Traversable[(Int, N)]): Vector[N]
  def outNeighbours(neuron: Int): Vector[Int]

}

trait Recurrent[N, W] {
  /**
   * Activating the network means computing the output values for all neurons simultaneously.
   * There is no order effect. The activation is done steps times. For the first activation, the
   * input neurones are initialised to the inputValues, and the other neurones to initialNeuronState
   */
  def activate(steps: Int, inputValues: Seq[N]): Vector[N] = {
    activateRec(steps, updateState(state, inputNeurons zip inputValues))
  }

  @tailrec private def activateRec(steps: Int, state: Vector[N]): Vector[N] =
    if (steps <= 0) state
    else activateRec(steps - 1, activateOnce(state))

  /** @return the number of steps, the average change over all node states during the last step, and the final state of the output neurons */
  def activateUntilStable(maxsteps: Int, stabilityThreshold: Double, inputValues: Seq[N]): (Int, Double, Vector[N]) = {
    activateUntilStableRec(maxsteps, stabilityThreshold, updateState(state, inputNeurons zip inputValues))
  }

  @tailrec private def activateUntilStableRec(maxsteps: Int, stabilityThreshold: Double, state: Vector[N], steps: Int = 0, avgchange: Double = 0.0): (Int, Double, Vector[N]) =
    if (steps >= maxsteps) (steps, avgchange, state)
    else {
      val nextstate = activateOnce(state)
      val newavgchange = nextstate.zip(state).foldLeft[Double](0.0) { (sum, twostates) => sum + change(twostates._1, twostates._2) } / state.length
      if (newavgchange < stabilityThreshold) (steps, newavgchange, nextstate)
      else activateUntilStableRec(maxsteps, stabilityThreshold, nextstate, steps + 1, newavgchange)
    }

  def activateOnce(state: Vector[N]): Vector[N] =
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
  def propagate(steps: Int, inputValues: Seq[N]): Vector[N] = {
    propagateRec(steps, updateState(state, inputNeurons zip inputValues), inputNeurons)
  }

  @tailrec private def propagateRec(steps: Int, state: Vector[N], currentNeurons: IndexedSeq[Int]): Vector[N] = {
    if (steps <= 0) {
      state
    } else {
      val (nextState, nextNeurons) = propagateOnce(state, currentNeurons)
      propagateRec(steps - 1, nextState, nextNeurons)
    }
  }

  // def propagateFullState(steps: Int, inputValues: Seq[N]): Vector[N] = {
  //   propagateFullStateRec(steps, updateState(inputNeurons zip inputValues), inputNeurons)
  //   (0 until steps).scanLeft[(Vector[N], Vector[Int]), List]((updateState(inputNeurons zip inputValues), inputNeurons))((lastState, step) =>
  //     propagateOnce())
  // }

  def propagateUntilStable(maxsteps: Int, stabilityThreshold: Double, inputValues: Seq[N]): (Int, Double, Vector[N]) = {
    propagateUntilStableRec(maxsteps, stabilityThreshold, updateState(state, inputNeurons zip inputValues), inputNeurons)
  }

  def propagateUntilStableRec(
    maxsteps: Int,
    stabilityThreshold: Double,
    state: Vector[N],
    currentNeurons: IndexedSeq[Int],
    step: Int = 0,
    avgchange: Double = 0.0): (Int, Double, Vector[N]) = {
    if (step >= maxsteps) (step, avgchange, state)
    else {
      val (nextState, nextNeurons) = propagateOnce(state, currentNeurons)
      val newavgchange =
        if (nextNeurons.length == 0) 0
        else nextNeurons.foldLeft[Double](0.0) { (sum, neuron) => sum + change(nextState(neuron), state(neuron)) } / nextNeurons.length
      if (newavgchange < stabilityThreshold) (step, newavgchange, nextState)
      else propagateUntilStableRec(
        maxsteps,
        stabilityThreshold,
        nextState,
        nextNeurons,
        step + 1,
        newavgchange)
    }
  }

  /** returns the nextstate resulting from activating the neurons leading out of those in currentNeurons, as those neurons */
  def propagateOnce(state: IndexedSeq[N], currentNeurons: IndexedSeq[Int]): (Vector[N], Vector[Int]) = {
    val nextNeurons = currentNeurons.toSet.flatMap { n: Int => outNeighbours(n) }.toVector
    val nextValues = nextNeurons.map { n =>
      val iaw = inputsAndWeights(n, state)
      if (iaw.length == 0) state(n)
      else activate(n, iaw)
    }
    (updateState(state, nextNeurons zip nextValues), nextNeurons)
  }

  def activate(neuron: Int, inputs: Traversable[(N, W)]): N // activate a single neuron
  def state: Vector[N]
  def inputNeurons: Vector[Int]
  def inputsAndWeights(neuron: Int, state: IndexedSeq[N]): Vector[(N, W)]
  def updateState(state: Traversable[N], neuronsValues: Traversable[(Int, N)]): Vector[N]
  def outNeighbours(neuron: Int): Vector[Int]
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

object NeuralNetwork {

  /**Create a neural network without cycles. This function makes no check on the topology of the graph (e.g. absence of cycles).*/
  def feedforwardNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: Traversable[(N, W)] => N,
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Feedforward[N, W] with HomogeneousActivationFunction[N, W] = {
    require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
    require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
    require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
    new NeuralNetwork[N, W] with Feedforward[N, W] with HomogeneousActivationFunction[N, W] {
      val network = Network.directedSparse(_nodes, _edges)
      val state: Vector[N] = _state.toVector
      val inputNeurons: Vector[Int] = _inputnodes.toVector
      val outputNeurons: Vector[Int] = _outputnodes.toVector
      val activationFunction = _activationfunction
    }
  }

  def feedforwardNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: IndexedSeq[Traversable[(N, W)] => N],
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Feedforward[N, W] with HeterogeneousActivationFunction[N, W] = {
    require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
    require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
    require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
    new NeuralNetwork[N, W] with Feedforward[N, W] with HeterogeneousActivationFunction[N, W] {
      val network = Network.directedSparse(_nodes, _edges)
      val state: Vector[N] = _state.toVector
      val inputNeurons: Vector[Int] = _inputnodes.toVector
      val outputNeurons: Vector[Int] = _outputnodes.toVector
      val activationFunction = _activationfunction
    }
  }

  def recurrentNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: Traversable[(N, W)] => N,
    _change: (N, N) => Double,
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Recurrent[N, W] with HomogeneousActivationFunction[N, W] = {
    require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
    require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
    require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
    new NeuralNetwork[N, W] with Recurrent[N, W] with HomogeneousActivationFunction[N, W] {
      val network = Network.directedSparse(_nodes, _edges)
      val state: Vector[N] = _state.toVector
      val inputNeurons: Vector[Int] = _inputnodes.toVector
      val outputNeurons: Vector[Int] = _outputnodes.toVector
      val activationFunction = _activationfunction
      def change(newstate: N, oldstate: N): Double = _change(newstate, oldstate)
    }
  }

  def recurrentNetwork[N, W](
    _nodes: Int,
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, W)],
    _activationfunction: IndexedSeq[Traversable[(N, W)] => N],
    _change: (N, N) => Double,
    _state: IndexedSeq[N]): NeuralNetwork[N, W] with Recurrent[N, W] with HeterogeneousActivationFunction[N, W] = {
    require(_inputnodes.forall { _ < _nodes }, "_inputnodes refer to nodes whose indices are bigger than _nodes")
    require(_outputnodes.forall { _ < _nodes }, "_outputnodes refer to nodes whose indices are bigger than _nodes")
    require(_edges.forall { case (u, v, _) => (u < _nodes) && (v < _nodes) }, "_edges refer to nodes whose indices are bigger than _nodes")
    new NeuralNetwork[N, W] with Recurrent[N, W] with HeterogeneousActivationFunction[N, W] {
      val network = Network.directedSparse(_nodes, _edges)
      val state: Vector[N] = _state.toVector
      val inputNeurons: Vector[Int] = _inputnodes.toVector
      val outputNeurons: Vector[Int] = _outputnodes.toVector
      val activationFunction = _activationfunction
      def change(newstate: N, oldstate: N): Double = _change(newstate, oldstate)
    }
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

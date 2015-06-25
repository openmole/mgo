/*
 * Copyright (C) Guillaume Chérel 2015
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

package test

import fr.iscpif.mgo.tools.network._
import fr.iscpif.mgo.tools.neuralnetwork._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.{ Gen, Properties }

// NeuralNetwork(inputs: Int, outputs: Int, bias: Boolean, edges: Seq[(innode,outnode)]).query(inputs): Seq[outputvalues]

// CPPN(inputs: Int = 4, outputs: Int = 1, bias: Boolean = true, nodes: Seq[ActivationFunctions], edges: Seq[(innode,outnode)]).query(inputs): Seq[outputvalues]

object NeuralNetworkSpecification extends Properties("NeuralNetwork") {
  def layers(l: Seq[Int]) =
    Vector.tabulate(l.size)(i => l.take(i).sum until l.take(i + 1).sum)

  def linkLayers(l1: Seq[Int], l2: Seq[Int]): Seq[(Int, Int)] =
    for {
      u <- l1
      v <- l2
    } yield (u, v)

  def perceptronTopology(inputs: Int, outputs: Int, hidden: Seq[Int]) = {
    val nodes: Seq[Seq[Int]] = layers(inputs +: hidden :+ outputs)
    (nodes.head, //input nodes
      nodes.last, //output nodes
      Vector.tabulate(nodes.size - 1)(i => linkLayers(nodes(i), nodes(i + 1))).flatten) //edges
  }

  val perceptron: Gen[(Seq[Int], Seq[Int], Seq[(Int, Int)])] = Gen.sized { size =>
    for {
      inputs <- Gen.choose(0, size)
      outputs <- Gen.choose(0, size)
      hiddenlayers <- Gen.choose(0, 5)
      hidden <- Gen.containerOfN[Vector, Int](hiddenlayers, Gen.choose(0, size))
    } yield perceptronTopology(inputs, outputs, hidden)
  }

  def perceptronWithWeights(inputs: Int, outputs: Int, hidden: Seq[Int]) = Gen.sized { size =>
    val (inputnodes, outputnodes, edges) = perceptronTopology(inputs, outputs, hidden)
    for {
      weights <- Gen.containerOfN[Seq, Double](edges.size, Gen.choose(-size, size))
    } yield (inputnodes, outputnodes, (edges zip weights) map { case (e, w) => (e._1, e._2, w) })
  }

  val perceptronWithWeights: Gen[(Seq[Int], Seq[Int], Seq[(Int, Int, Double)])] = Gen.sized { size =>
    for {
      inputs <- Gen.choose(0, size)
      outputs <- Gen.choose(0, size)
      hiddenlayers <- Gen.choose(0, 5)
      hidden <- Gen.containerOfN[Seq, Int](hiddenlayers, Gen.choose(0, size))
      (inputnodes, outputnodes, edges) <- perceptronWithWeights(inputs, outputs, hidden)
    } yield (inputnodes, outputnodes, edges)
  }

  val activationFunctionDoubleDouble: Gen[Traversable[(Double, Double)] => Double] = Gen.oneOf(ActivationFunction.tanh, ActivationFunction.logistic)

  property("HomogeneousActivationFunction") =
    forAll(
      activationFunctionDoubleDouble,
      Gen.containerOf[Vector, Vector[(Double, Double)]](
        Gen.containerOf[Vector, (Double, Double)](for { a <- Gen.choose(-100.0, 100.0); b <- Gen.choose(-100.0, 100.0) } yield (a, b)))) {
        (af, inp) =>
          val haf = new HomogeneousActivationFunction[Double, Double] {
            def activationFunction: Traversable[(Double, Double)] => Double = af
          }
          all(inp.zipWithIndex.map { case (inputsAndWeights, neuron) => haf.activate(neuron, inp(neuron)) ?= af(inputsAndWeights) }: _*)
      }

  property("HeterogeneousActivationFunction") =
    forAll(
      Gen.containerOf[Vector, Traversable[(Double, Double)] => Double](activationFunctionDoubleDouble)) { af =>
        forAll(
          Gen.containerOfN[Vector, Vector[(Double, Double)]](
            af.size,
            Gen.containerOf[Vector, (Double, Double)](
              for { a <- Gen.choose(-100.0, 100.0); b <- Gen.choose(-100.0, 100.0) } yield (a, b)))) { inp =>
            val haf = new HeterogeneousActivationFunction[Double, Double] {
              def activationFunction = af
            }
            all(inp.zipWithIndex.map { case (inputsAndWeights, neuron) => haf.activate(neuron, inp(neuron)) ?= af(neuron)(inputsAndWeights) }: _*)
          }
      }

  /** a class containing a neural network topology which realises the associated input/output values*/
  case class NeuralNetworkTestSetting[N, W](
    network: Network[Unit, W] with DirectedEdges[W],
    inputNeurons: Vector[Int],
    outputNeurons: Vector[Int],
    activationFunction: Either[Traversable[(N, W)] => N, IndexedSeq[Traversable[(N, W)] => N]],
    inputsAndOutputs: Seq[(Seq[Double], Seq[Double])],
    requiredSteps: Int)

  val xorSettings: List[NeuralNetworkTestSetting[Double, Double]] =
    List(
      NeuralNetworkTestSetting(
        network = Network.directedSparse(
          5,
          Vector(
            (0, 2, 1.0),
            (0, 3, -1.0),
            (1, 2, -1.0),
            (1, 3, 1.0),
            (2, 4, 1.0),
            (3, 4, 1.0))),
        inputNeurons = Vector[Int](0, 1),
        outputNeurons = Vector[Int](4),
        activationFunction = Left(ActivationFunction.heaviside(0)),
        inputsAndOutputs = Vector(
          (Vector(0.0, 0.0), Vector(0.0)),
          (Vector(0.0, 1.0), Vector(1.0)),
          (Vector(1.0, 0.0), Vector(1.0)),
          (Vector(1.0, 1.0), Vector(0.0))),
        requiredSteps = 2),
      NeuralNetworkTestSetting(
        network = Network.directedSparse(
          4,
          Vector(
            (0, 2, 1.0),
            (0, 3, -2.0),
            (1, 2, 1.0),
            (1, 3, -2.0),
            (2, 3, 4.0))),
        inputNeurons = Vector[Int](0, 1),
        outputNeurons = Vector[Int](3),
        activationFunction = Right(Vector(
          ActivationFunction.zero,
          ActivationFunction.zero,
          ActivationFunction.heaviside(0),
          ActivationFunction.heaviside(0))),
        inputsAndOutputs = Vector(
          (Vector(0.0, 0.0), Vector(0.0)),
          (Vector(0.0, 1.0), Vector(1.0)),
          (Vector(1.0, 0.0), Vector(1.0)),
          (Vector(1.0, 1.0), Vector(0.0))),
        requiredSteps = 2)
    )

  property("Feedforward query XOR") =
    forAll(Gen.oneOf(xorSettings.indices)) { xorset =>
      val setting = xorSettings(xorset)
      val feedforward: Feedforward[Double, Double] with NeuralNetwork[Double, Double] =
        setting.activationFunction match {
          case Left(af) => new Feedforward[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
            def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
            def inputNeurons: Vector[Int] = setting.inputNeurons
            def outputNeurons: Vector[Int] = setting.outputNeurons
            val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
            val activationFunction: Traversable[(Double, Double)] => Double = af
            def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
          }
          case Right(af) => new Feedforward[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
            def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
            def inputNeurons: Vector[Int] = setting.inputNeurons
            def outputNeurons: Vector[Int] = setting.outputNeurons
            val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
            val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
            def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
          }
        }
      all(setting.inputsAndOutputs.map {
        case (inputValues, outputValues) => (feedforward.outputState(feedforward.query(inputValues)) ?= outputValues.toVector) :|
          s"inputs = $inputValues" :|
          s"expected output = $outputValues"
      }: _*)
    }

  property("Recurrent activate XOR steps >= 2") =
    forAll(Gen.oneOf(xorSettings.indices)) { xorset =>
      val setting = xorSettings(xorset)
      forAll(Gen.choose(setting.requiredSteps, setting.requiredSteps + 100)) { steps =>
        val setting = xorSettings(xorset)
        val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
          setting.activationFunction match {
            case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: Traversable[(Double, Double)] => Double = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
            case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
          }
        all(setting.inputsAndOutputs.map {
          case (inputValues, outputValues) => (recurrent.outputState(recurrent.activate(steps, inputValues)) ?= outputValues.toVector) :|
            s"inputs = $inputValues" :|
            s"expected output = $outputValues" :|
            s"steps = $steps"
        }: _*)
      }
    }

  property("Recurrent activate until stable XOR") =
    forAll(Gen.oneOf(xorSettings.indices)) { xorset =>
      val setting = xorSettings(xorset)
      forAll(Gen.choose(setting.requiredSteps + 1, setting.requiredSteps + 1 + 20), Gen.choose(0.000001, 0.1)) { (maxsteps, stabilityThreshold) =>
        val setting = xorSettings(xorset)
        val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
          setting.activationFunction match {
            case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: Traversable[(Double, Double)] => Double = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
            case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
          }
        all(setting.inputsAndOutputs.map {
          case (inputValues, outputValues) => {
            val (steps, change, newState) = recurrent.activateUntilStable(maxsteps, stabilityThreshold, inputValues)
            val newOutputValues = recurrent.outputState(newState)
            ((steps <= 3) :| s"took $steps steps" &&
              (change < stabilityThreshold) :| s"change = $change" &&
              (newOutputValues ?= outputValues.toVector)) :|
              s"inputs = $inputValues" :|
              s"output = $newOutputValues" :|
              s"expected output = $outputValues" :|
              s"change = $change" :|
              s"steps = $steps" :|
              s"maxsteps = $maxsteps" :|
              s"stabilityThreshold = $stabilityThreshold"
          }
        }: _*)
      }
    }

  property("Recurrent propagate XOR") =
    forAll(Gen.oneOf(xorSettings.indices)) { xorset =>
      val setting = xorSettings(xorset)
      forAll(Gen.choose(setting.requiredSteps, setting.requiredSteps + 100)) { steps =>
        val setting = xorSettings(xorset)
        val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
          setting.activationFunction match {
            case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: Traversable[(Double, Double)] => Double = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
            case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
          }
        all(setting.inputsAndOutputs.map {
          case (inputValues, outputValues) =>
            val finalstate = recurrent.propagate(steps, inputValues)
            (recurrent.outputState(finalstate) ?= outputValues.toVector) :|
              s"xorset = $xorset" :|
              s"inputs = $inputValues" :|
              s"expected output = $outputValues" :|
              s"steps = $steps" :|
              s"finalstate = $finalstate"
        }: _*)
      }
    }

  property("Recurrent propagate until stable XOR") =
    forAll(Gen.oneOf(xorSettings.indices)) { xorset =>
      val setting = xorSettings(xorset)
      forAll(Gen.choose(setting.requiredSteps + 1, setting.requiredSteps + 1 + 20), Gen.choose(0.000001, 0.1)) { (maxsteps, stabilityThreshold) =>
        val setting = xorSettings(xorset)
        val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
          setting.activationFunction match {
            case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: Traversable[(Double, Double)] => Double = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
            case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
              def state: Vector[Double] = setting.network.nodes.map { _ => 0.0 }
              def inputNeurons: Vector[Int] = setting.inputNeurons
              def outputNeurons: Vector[Int] = setting.outputNeurons
              val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
              val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
              def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
            }
          }
        all(setting.inputsAndOutputs.map {
          case (inputValues, outputValues) => {
            val (steps, change, newState) = recurrent.propagateUntilStable(maxsteps, stabilityThreshold, inputValues)
            val newOutputValues = recurrent.outputState(newState)
            ((steps <= 3) :| s"took $steps steps" &&
              (change < stabilityThreshold) :| s"change = $change" &&
              (newOutputValues ?= outputValues.toVector)) :|
              s"inputs = $inputValues" :|
              s"output = $newOutputValues" :|
              s"expected output = $outputValues" :|
              s"change = $change" :|
              s"steps = $steps" :|
              s"maxsteps = $maxsteps" :|
              s"stabilityThreshold = $stabilityThreshold"
          }
        }: _*)
      }
    }

  def countingLoopNetwork(neurons: Int) =
    NeuralNetwork.recurrentNetwork(
      _nodes = neurons,
      _inputnodes = (0 until neurons).toVector,
      _outputnodes = (0 until neurons).toVector,
      _edges = (0 until neurons).flatMap(n => (0 until neurons).map { v => (n, v, 1.0) }).toVector,
      _activationfunction = (inputsAndWeights: Traversable[(Double, Double)]) => inputsAndWeights.foldLeft(0.0) { case (sum, (input, weight)) => sum + (input + weight) },
      _change = ChangeFunction.absoluteDifference,
      _state = (0 until neurons).map { _ => 0.0 }.toVector)

  def countingLoopExpectedFinalState(steps: Int, nnodes: Int, x: Double = 0): Double =
    if (steps <= 0) x
    else countingLoopExpectedFinalState(steps - 1, nnodes, (x + 1) * nnodes)

  property("Recurrent counting loop activate") =
    forAll(Gen.choose(0, 10), Gen.choose(0, 10)) { (steps, neurons) =>
      val neuralnet = countingLoopNetwork(neurons)
      (neuralnet.activate(steps, (0 until neurons).map { _ => 0.0 }.toVector).sum ?= countingLoopExpectedFinalState(steps, neurons) * neurons) :|
        s"steps = $steps" :|
        s"neurons = $neurons" :|
        s"neuron indices: ${(0 until neurons).toVector}" :|
        s"inputnodes = ${neuralnet.inputNeurons}" :|
        s"edges = ${neuralnet.network.iteredges.toVector}"
    }

  property("Recurrent counting loop propagate") =
    forAll(Gen.choose(0, 10), Gen.choose(0, 10)) { (steps, neurons) =>
      val neuralnet = countingLoopNetwork(neurons)
      (neuralnet.propagate(steps, (0 until neurons).map { _ => 0.0 }.toVector).sum ?= countingLoopExpectedFinalState(steps, neurons) * neurons) :|
        s"steps = $steps" :|
        s"neurons = $neurons" :|
        s"neuron indices: ${(0 until neurons).toVector}" :|
        s"inputnodes = ${neuralnet.inputNeurons}" :|
        s"edges = ${neuralnet.network.iteredges.toVector}"
    }

  property("NeuralNetwork stateWithInputs") =
    forAll(Gen.containerOf[Vector, Double](arbitrary[Double])) { tstate =>
      forAll(Gen.someOf(tstate.indices)) { tInputNodes =>
        forAll(Gen.containerOfN[Seq, Double](tInputNodes.length, arbitrary[Double])) { tInputValues =>
          val nn = new NeuralNetwork[Double, Double] {
            val inputNeurons: Vector[Int] = tInputNodes.toVector
            val state = tstate

            def network = ???
            def outputNeurons = ???
          }
          val swi = nn.updateState(nn.state, nn.inputNeurons zip tInputValues)
          ((tInputNodes zip tInputValues) forall { case (n, v) => swi(n) == v }) :| "inputs updated" &&
            ((swi.zipWithIndex) forall { case (v, n) => if (!tInputNodes.contains(n)) v == tstate(n) else true }) :| "non input nodes unchanged"

        }
      }
    }

}

/*
 * Copyright (C) Guillaume Chérel 2/05/14
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

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import fr.iscpif.mgo.tools.network._
import fr.iscpif.mgo.tools.neuralnetwork._

import math.tanh

// NeuralNetwork(inputs: Int, outputs: Int, bias: Boolean, edges: Seq[(innode,outnode)]).query(inputs): Seq[outputvalues]

// CPPN(inputs: Int = 4, outputs: Int = 1, bias: Boolean = true, nodes: Seq[ActivationFunctions], edges: Seq[(innode,outnode)]).query(inputs): Seq[outputvalues]

object NeuralNetworkSpecification extends Properties("NeuralNetwork") {
  def layers(l: Seq[Int]) =
    Vector.tabulate(l.size)(i => (l.take(i).sum) until (l.take(i + 1).sum))

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
    } yield (perceptronTopology(inputs, outputs, hidden))
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

  val xorNetwork: Network[Unit, Double] with DirectedEdges[Double] with SparseTopology[Double] =
    Network.directedSparse(
      5,
      Vector(
        (0, 2, 1.0),
        (0, 3, -1.0),
        (1, 2, -1.0),
        (1, 3, 1.0),
        (2, 4, 1.0),
        (3, 4, 1.0)
      ))

  val xorInputOutput: Seq[(Seq[Double], Seq[Double])] =
    Vector(
      (Vector(0.0, 0.0), Vector(0.0)),
      (Vector(0.0, 1.0), Vector(1.0)),
      (Vector(1.0, 0.0), Vector(1.0)),
      (Vector(1.0, 1.0), Vector(0.0)))

  /** a class containing a neural network topology which realises the associated input/output values*/
  case class NeuralNetworkTestSetting[N, W](
    network: Network[Unit, W] with DirectedEdges[W],
    inputNeurons: IndexedSeq[Int],
    outputNeurons: IndexedSeq[Int],
    activationFunction: Either[Traversable[(N, W)] => N, IndexedSeq[Traversable[(N, W)] => N]],
    inputsAndOutputs: Seq[(Seq[Double], Seq[Double])])

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
          (Vector(1.0, 1.0), Vector(0.0)))),
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
          (Vector(1.0, 1.0), Vector(0.0))))
    )

  property("Feedforward query XOR") = false

  property("Recurrent activate XOR steps >= 2") = forAll(Gen.oneOf(xorSettings.indices), Gen.choose(2, 100)) { (xorset, steps) =>
    val setting = xorSettings(xorset)
    val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
      setting.activationFunction match {
        case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
          def state: IndexedSeq[Double] = setting.network.nodes.map { _ => 0.0 }
          def inputNeurons: IndexedSeq[Int] = setting.inputNeurons
          def outputNeurons: IndexedSeq[Int] = setting.outputNeurons
          val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
          val activationFunction: Traversable[(Double, Double)] => Double = af
          def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
        }
        case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
          def state: IndexedSeq[Double] = setting.network.nodes.map { _ => 0.0 }
          def inputNeurons: IndexedSeq[Int] = setting.inputNeurons
          def outputNeurons: IndexedSeq[Int] = setting.outputNeurons
          val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
          val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
          def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
        }
      }
    all(xorInputOutput.map {
      case (inputValues, outputValues) => (recurrent.activate(steps, inputValues) ?= outputValues.toVector) :|
        s"inputs = $inputValues" :|
        s"expected output = $outputValues" :|
        s"steps = $steps"
    }: _*)
  }

  property("Recurrent activate until stable XOR") = forAll(Gen.oneOf(xorSettings.indices), Gen.choose(3, 20), Gen.choose(0.000001, 0.1)) { (xorset, maxsteps, stabilityThreshold) =>
    val setting = xorSettings(xorset)
    val recurrent: Recurrent[Double, Double] with NeuralNetwork[Double, Double] =
      setting.activationFunction match {
        case Left(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HomogeneousActivationFunction[Double, Double] {
          def state: IndexedSeq[Double] = setting.network.nodes.map { _ => 0.0 }
          def inputNeurons: IndexedSeq[Int] = setting.inputNeurons
          def outputNeurons: IndexedSeq[Int] = setting.outputNeurons
          val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
          val activationFunction: Traversable[(Double, Double)] => Double = af
          def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
        }
        case Right(af) => new Recurrent[Double, Double] with NeuralNetwork[Double, Double] with HeterogeneousActivationFunction[Double, Double] {
          def state: IndexedSeq[Double] = setting.network.nodes.map { _ => 0.0 }
          def inputNeurons: IndexedSeq[Int] = setting.inputNeurons
          def outputNeurons: IndexedSeq[Int] = setting.outputNeurons
          val network: Network[Unit, Double] with DirectedEdges[Double] = setting.network
          val activationFunction: IndexedSeq[Traversable[(Double, Double)] => Double] = af
          def change(newstate: Double, oldstate: Double): Double = ChangeFunction.absoluteDifference(newstate, oldstate)
        }
      }
    all(xorInputOutput.map {
      case (inputValues, outputValues) => {
        val (steps, change, newOutputValues) = recurrent.activateUntilStable(maxsteps, stabilityThreshold, inputValues)
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

  property("Recurrent propagate XOR") = false

  property("Recurrent propagate until stable XOR") = false

  property("Recurrent recurrent loops (hopfield net)") = false

  property("NeuralNetwork stateWithInputs") =
    forAll(Gen.containerOf[IndexedSeq, Double](arbitrary[Double])) { tstate =>
      forAll(Gen.someOf(tstate.indices)) { tInputNodes =>
        forAll(Gen.containerOfN[Seq, Double](tInputNodes.length, arbitrary[Double])) { tInputValues =>
          val nn = new NeuralNetwork[Double, Double] {
            val inputNeurons: IndexedSeq[Int] = tInputNodes.toIndexedSeq
            val state = tstate

            def network = ???
            def outputNeurons = ???
          }
          val swi = nn.stateWithInputs(tInputValues)
          ((tInputNodes zip tInputValues) forall { case (n, v) => swi(n) == v }) :| "inputs updated" &&
            ((swi.zipWithIndex) forall { case (v, n) => if (!tInputNodes.contains(n)) v == tstate(n) else true }) :| "non input nodes unchanged"

        }
      }
    }

  property("NeuralNetwork feedforward 0.0 inputs/outputs with tanh activation function") = false
  // forAll(perceptronWithWeights) {
  //   (p) =>
  //     {
  //       val inputnodes = p._1
  //       val outputnodes = p._2
  //       val edges = p._3
  //       (inputnodes.size > 0 && outputnodes.size > 0) ==> {
  //         val outputvalues = NeuralNetwork.feedforwardNetwork(inputnodes, outputnodes, false, edges, ActivationFunction.tanh).query(inputnodes.map { _ => 0.0 })
  //         all(outputvalues.map(Compare.doubles(_, 0.0)): _*)
  //       }
  //     }
  // }

  // property("NeuralNetwork feedforward negative or positive inputs/outputs with tanh activation function") =
  //   forAll(perceptronWithWeights) {
  //     (p) =>
  //       {
  //         val inputnodes = p._1
  //         val outputnodes = p._2
  //         val edges = p._3
  //         (inputnodes.size > 0 && outputnodes.size > 0) ==> {
  //           val outputvalues = NeuralNetwork.feedforwardNetwork(inputnodes, outputnodes, false, edges, ActivationFunction.tanh).feedForwardOnce(inputnodes.map { _ => 0.0 })
  //           all(outputvalues.map(Compare.doubles(_, 0.0)): _*)
  //         }
  //       }
  //   }

}

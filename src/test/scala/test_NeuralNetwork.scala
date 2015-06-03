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

  property("NeuralNetwork feedforward 0.0 inputs/outputs with tanh activation function") =
    forAll(perceptronWithWeights) {
      (p) =>
        {
          val inputnodes = p._1
          val outputnodes = p._2
          val edges = p._3
          (inputnodes.size > 0 && outputnodes.size > 0) ==> {
            val outputvalues = NeuralNetwork.feedforwardNetwork(inputnodes, outputnodes, false, edges, ActivationFunction.tanh).query(inputnodes.map { _ => 0.0 })
            all(outputvalues.map(Compare.doubles(_, 0.0)): _*)
          }
        }
    }

  // property("NeuralNetwork feedforward negative or positive inputs/outputs iwth tanh activation function") =
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

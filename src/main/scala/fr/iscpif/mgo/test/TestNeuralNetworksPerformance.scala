///*
// * Copyright (C) 08/07/2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.test
//
//import fr.iscpif.mgo.tools.neuralnetwork.{ ChangeFunction, ActivationFunction, NeuralNetwork }
//import scala.collection.mutable.ArrayBuffer
//import util.Random
//
//import fr.iscpif.mgo.tools.time
//import breeze.linalg.{ DenseMatrix, DenseVector }
//
//object TestNeuralNetworksPerformance {
//  val rng = new Random()
//
//  def main(args: Array[String]) {
//
//    val inputs = 100
//    val outputs = 10
//    val hidden = Vector[Int](10)
//    val activations = 1 + hidden.length
//
//    val activationFunction = ActivationFunction.logistic
//    val changeFunction = ChangeFunction.absoluteDifference _
//    val baseState = 0.0
//
//    val nodes = (0 until inputs + outputs + hidden.sum).toVector
//    val (inputNodes, outputNodes, edges) = layeredTopology(inputs, outputs, hidden)
//    val edgesWithWeights = edges.map { case (u, v) => (u, v, rng.nextDouble() * 10 - 5) }
//
//    val edgesMatrixArray = ArrayBuffer.fill(nodes.length)(ArrayBuffer.fill(nodes.length)(0.0))
//    edgesWithWeights.foreach { case (u, v, e) => edgesMatrixArray(u)(v) = e }
//    val edgesMatrix = edgesMatrixArray.map { _.toVector }.toVector
//
//    println("-- Feedforward sparse --")
//    val nnfs =
//      time("Creation",
//        NeuralNetwork.feedforwardSparse(
//          nodes,
//          inputNodes,
//          outputNodes,
//          edgesWithWeights,
//          activationFunction,
//          nodes.map { _ => baseState }
//        ))
//    time("Query", nnfs.outputState(nnfs.query(nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Query", nnfs.outputState(nnfs.query(nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Query", nnfs.outputState(nnfs.query(nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Query", nnfs.outputState(nnfs.query(nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Query", nnfs.outputState(nnfs.query(nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//
//    println("-- Recurrent sparse --")
//    val nnrs =
//      time("Creation",
//        NeuralNetwork.recurrentSparse(
//          nodes,
//          inputNodes,
//          outputNodes,
//          edgesWithWeights,
//          activationFunction,
//          changeFunction,
//          nodes.map { _ => baseState }
//        ))
//    time("Activate", nnrs.outputState(nnrs.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrs.outputState(nnrs.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrs.outputState(nnrs.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrs.outputState(nnrs.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrs.outputState(nnrs.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//
//    time("Activate until stable", nnrs.outputState(nnrs.activateUntilStable(activations, 0.0, nodes.map { _ => rng.nextDouble() * 2 - 1 })._3))
//    time("Activate until stable", nnrs.outputState(nnrs.activateUntilStable(activations, 0.0, nodes.map { _ => rng.nextDouble() * 2 - 1 })._3))
//    time("Activate until stable", nnrs.outputState(nnrs.activateUntilStable(activations, 0.0, nodes.map { _ => rng.nextDouble() * 2 - 1 })._3))
//    time("Activate until stable", nnrs.outputState(nnrs.activateUntilStable(activations, 0.0, nodes.map { _ => rng.nextDouble() * 2 - 1 })._3))
//    time("Activate until stable", nnrs.outputState(nnrs.activateUntilStable(activations, 0.0, nodes.map { _ => rng.nextDouble() * 2 - 1 })._3))
//
//    println("-- Recurrent dense --")
//    val nnrd =
//      time("Creation",
//        NeuralNetwork.recurrentDense(
//          nodes,
//          inputNodes,
//          outputNodes,
//          edgesMatrix,
//          activationFunction,
//          changeFunction,
//          nodes.map { _ => baseState }
//        ))
//    time("Activate", nnrd.outputState(nnrd.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrd.outputState(nnrd.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrd.outputState(nnrd.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrd.outputState(nnrd.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//    time("Activate", nnrd.outputState(nnrd.activate(activations, nodes.map { _ => rng.nextDouble() * 2 - 1 })))
//
//    println("-- Recurrent breeze --")
//    time("Breeze loads stuff", DenseVector(0.0, 1.0, 2.0) * DenseVector(0.0, 1.0, 2.0).t)
//    val breezeEdges = DenseMatrix(edgesMatrix: _*)
//    time("Activate", {
//      val finalstate = Iterator.iterate(DenseVector[Double](nodes.map { _ => rng.nextDouble() * 2 - 1 }: _*)) { s => breezeEdges * s }.drop(activations).next
//      outputNodes.map { finalstate(_) }
//    })
//    time("Activate", {
//      val finalstate = Iterator.iterate(DenseVector[Double](nodes.map { _ => rng.nextDouble() * 2 - 1 }: _*)) { s => breezeEdges * s }.drop(activations).next
//      outputNodes.map { finalstate(_) }
//    })
//    time("Activate", {
//      val finalstate = Iterator.iterate(DenseVector[Double](nodes.map { _ => rng.nextDouble() * 2 - 1 }: _*)) { s => breezeEdges * s }.drop(activations).next
//      outputNodes.map { finalstate(_) }
//    })
//    time("Activate", {
//      val finalstate = Iterator.iterate(DenseVector[Double](nodes.map { _ => rng.nextDouble() * 2 - 1 }: _*)) { s => breezeEdges * s }.drop(activations).next
//      outputNodes.map { finalstate(_) }
//    })
//    time("Activate", {
//      val finalstate = Iterator.iterate(DenseVector[Double](nodes.map { _ => rng.nextDouble() * 2 - 1 }: _*)) { s => breezeEdges * s }.drop(activations).next
//      outputNodes.map { finalstate(_) }
//    })
//
//  }
//
//  /** Edges of a layered network. */
//  def layeredTopology(inputs: Int, outputs: Int, hidden: Seq[Int]) = {
//    val nodes: Seq[IndexedSeq[Int]] = layers(inputs +: hidden :+ outputs)
//    (nodes.head, //input nodes
//      nodes.last, //output nodes
//      Vector.tabulate(nodes.size - 1)(i => linkLayers(nodes(i), nodes(i + 1))).flatten) //edges
//  }
//
//  /** Returns nodes indices for each layer */
//  def layers(l: Seq[Int]): Seq[Vector[Int]] =
//    Vector.tabulate(l.size)(i => (l.take(i).sum until l.take(i + 1).sum).toVector)
//
//  /** Connects all nodes between l1 and l2 and return corresponding edges. */
//  def linkLayers(l1: Seq[Int], l2: Seq[Int]): Seq[(Int, Int)] =
//    for {
//      u <- l1
//      v <- l2
//    } yield (u, v)
//
//}

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

package fr.iscpif.mgo.problem

import fr.iscpif.mgo._
import scala.util.Random
import monocle.syntax._
import fr.iscpif.mgo.tools.neuralnetwork._
import collection.immutable.IntMap
import fr.iscpif.mgo.genome.NEATGenome.Node

/**
 * Cake to define a problem for a genetic algorithm
 */
trait NEATProblem extends Problem with NoPhenotype with NEATGenome with DoubleFitness {

  type NN <: NeuralNetwork[Double, Double, Double]

  def evaluate(phenotype: P, rng: Random): F = evaluateNet(createNet(phenotype)(rng))(rng)

  def createNet(phenotype: P)(implicit rng: Random): NN = {
    // nodes indices in phenotypes may not be contiguous. Translate into contiguous indices
    val nodes = phenotype.nodes.toVector.sortBy { case (index, node) => index }
    val nodesMap = IntMap.empty ++ nodes.map { case (index, _) => index }.toSeq.sorted.zipWithIndex
    createNet(
      nodes.map { case (originalindex, node) => node.level },
      (inputNodesIndices ++ biasNodesIndices).map { nodesMap(_) },
      outputNodesIndices.map { nodesMap(_) },
      phenotype.connectionGenes.filter { _.enabled }.map { cg => (nodesMap(cg.inNode), nodesMap(cg.outNode), cg.weight) },
      activationFunction _,
      IndexedSeq.fill(nodesMap.size)(neuronInitValue)
    )
  }

  def createNet(
    _nodes: IndexedSeq[Double],
    _inputnodes: IndexedSeq[Int],
    _outputnodes: IndexedSeq[Int],
    _edges: Seq[(Int, Int, Double)],
    _activationfunction: Traversable[(Double, Double)] => Double,
    _state: IndexedSeq[Double])(implicit rng: Random): NN

  def evaluateNet(n: NN)(implicit rng: Random): Double

  def activationFunction(inputsAndWeights: Traversable[(Double, Double)]): Double
  def neuronInitValue: Double
}

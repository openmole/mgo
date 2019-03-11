///*
// * Copyright (C) 2015 Guillaume Chérel
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
//package mgo.evolution.genome
//
//import scala.util.Random
//import monocle.Lens
//import mgo.evolution._
//import mgo.evolution.breed._
//import collection.immutable.IntMap
//
//object NEATGenome {
//
//  case class Genome[+NODEDATA](
//      connectionGenes: Seq[ConnectionGene],
//      nodes: IntMap[Node[NODEDATA]],
//      species: Int,
//      lastNodeId: Int) {
//
//    override def toString: String = s"Species $species; ${connectionGenes.toString}; $nodes"
//
//  }
//
//  case class ConnectionGene(
//      inNode: Int,
//      outNode: Int,
//      weight: Double,
//      enabled: Boolean,
//      innovation: Int) {
//    override def toString: String = f"$inNode%d${if (enabled) "-" else "X"}%s>$outNode%d($weight%.2f);$innovation%s"
//  }
//
//  sealed trait Node[+NODEDATA] { val level: Double; val data: NODEDATA }
//  case class InputNode[+NODEDATA](data: NODEDATA, level: Double = 0) extends Node[NODEDATA]
//  case class OutputNode[+NODEDATA](data: NODEDATA, level: Double = 1) extends Node[NODEDATA]
//  case class HiddenNode[+NODEDATA](data: NODEDATA, level: Double) extends Node[NODEDATA]
//  case class BiasNode[+NODEDATA](data: NODEDATA, level: Double = 0) extends Node[NODEDATA]
//
//  sealed trait Innovation
//  case class NodeInnovation[+NODEDATA](innovnum1: Int, innovnum2: Int, newnodeId: Int, newnode: Node[NODEDATA], inNode: Int, outNode: Int) extends Innovation
//  case class LinkInnovation(innovnum: Int, inNode: Int, outNode: Int) extends Innovation
//
//}
//
///**
// * Genome for NEAT
// */
//trait NEATGenome extends G {
//  type NODEDATA
//  type G = NEATGenome.Genome[NODEDATA]
//  type Node = NEATGenome.Node[NODEDATA]
//  type InputNode = NEATGenome.InputNode[NODEDATA]
//  type OutputNode = NEATGenome.OutputNode[NODEDATA]
//  type HiddenNode = NEATGenome.HiddenNode[NODEDATA]
//  type BiasNode = NEATGenome.BiasNode[NODEDATA]
//  type Innovation = NEATGenome.Innovation
//  type NodeInnovation = NEATGenome.NodeInnovation[NODEDATA]
//  type LinkInnovation = NEATGenome.LinkInnovation
//  type ConnectionGene = NEATGenome.ConnectionGene
//
//  def inputNodes: Int
//  def outputNodes: Int
//  def biasNodes: Int
//
//  def inputNodesIndices: Range = 0 until inputNodes
//  def biasNodesIndices: Range = inputNodes until inputNodes + biasNodes
//  def outputNodesIndices: Range = inputNodes + biasNodes until inputNodes + biasNodes + outputNodes
//
//}

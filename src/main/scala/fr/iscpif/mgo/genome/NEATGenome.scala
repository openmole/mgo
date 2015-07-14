/*
 * Copyright (C) 2015 Guillaume Chérel
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif.mgo.genome

import scala.util.Random
import monocle.Lens
import fr.iscpif.mgo._
import fr.iscpif.mgo.breed._
import collection.immutable.IntMap

/**
 * Genome for NEAT
 */
trait NEATGenome extends G {
  type NODEDATA
  type G = Genome

  def inputNodes: Int
  def outputNodes: Int
  def biasNodes: Int

  def inputNodesIndices: Range = 0 until inputNodes
  def biasNodesIndices: Range = inputNodes until inputNodes + biasNodes
  def outputNodesIndices: Range = inputNodes + biasNodes until inputNodes + biasNodes + outputNodes

  sealed trait Node { val level: Double; val data: NODEDATA }
  case class InputNode(data: NODEDATA, level: Double = 0) extends Node
  case class OutputNode(data: NODEDATA, level: Double = 1) extends Node
  case class HiddenNode(data: NODEDATA, level: Double) extends Node
  case class BiasNode(data: NODEDATA, level: Double = 0) extends Node

  case class Genome(
      connectionGenes: Seq[ConnectionGene],
      nodes: IntMap[Node],
      species: Int,
      lastNodeId: Int) {

    override def toString: String = s"Species $species; ${connectionGenes.toString}; $nodes"

  }

  case class ConnectionGene(
      inNode: Int,
      outNode: Int,
      weight: Double,
      enabled: Boolean,
      innovation: Int) {
    override def toString: String = f"$inNode%d${if (enabled) "-" else "X"}%s>$outNode%d($weight%.2f);$innovation%s"
  }

  sealed trait Innovation
  case class NodeInnovation(innovnum1: Int, innovnum2: Int, newnodeId: Int, newnode: Node, inNode: Int, outNode: Int) extends Innovation
  case class LinkInnovation(innovnum: Int, inNode: Int, outNode: Int) extends Innovation

}

//object NEATGenome {

// sealed trait Innovation {
//   def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[Innovation]): (Innovation, Int, Seq[Innovation])
//   def setNumber(x: Int): Innovation
//   def unsetNumber: UnnumberedInnovation
//   def sameAs(x: Innovation): Boolean
// }
// sealed trait UnnumberedInnovation extends Innovation {
//   def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[Innovation]): (Innovation, Int, Seq[Innovation]) =
//     recordOfInnovations.find { sameAs(_) } match {
//       case Some(oldinnov) => (setNumber(oldinnov.number), globalInnovationNumber, recordOfInnovations)
//       case None => {
//         val innovWithNumber = setNumber(globalInnovationNumber + 1)
//         (innovWithNumber, globalInnovationNumber + 1, recordOfInnovations :+ innovWithNumber)
//       }
//     }
// }
// sealed trait Innovation extends Innovation {
//   val number: Int
//   def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[Innovation]): (Innovation, Int, Seq[Innovation]) =
//     (this, globalInnovationNumber, recordOfInnovations)
// }
// sealed trait LinkInnovation extends Innovation {
//   val innode: Int
//   val outnode: Int
//   def setNumber(x: Int): NumberedLinkInnovation = NumberedLinkInnovation(x, innode, outnode)
//   def unsetNumber: UnnumberedLinkInnovation = UnnumberedLinkInnovation(innode, outnode)
//   def sameAs(x: Innovation): Boolean =
//     x match {
//       case x: LinkInnovation => (innode == x.innode) && (outnode == x.outnode)
//       case _ => false
//     }
// }
// sealed trait NodeInnovation extends Innovation {
//   val innode: Int
//   val outnode: Int
//   val splittedLinkInnovationNumber: Int
//   def setNumber(x: Int): NumberedNodeInnovation = NumberedNodeInnovation(x, innode, outnode, splittedLinkInnovationNumber)
//   def unsetNumber: UnnumberedNodeInnovation = UnnumberedNodeInnovation(innode, outnode, splittedLinkInnovationNumber)
//   def sameAs(x: Innovation): Boolean =
//     x match {
//       case x: NodeInnovation => (innode == x.innode) && (outnode == x.outnode) && (splittedLinkInnovationNumber == x.splittedLinkInnovationNumber)
//       case _ => false
//     }
// }
// case class UnnumberedLinkInnovation(innode: Int, outnode: Int) extends LinkInnovation with UnnumberedInnovation
// case class NumberedLinkInnovation(number: Int, innode: Int, outnode: Int) extends LinkInnovation with Innovation
// case class UnnumberedNodeInnovation(innode: Int, outnode: Int, splittedLinkInnovationNumber: Int) extends NodeInnovation with UnnumberedInnovation
// case class NumberedNodeInnovation(number: Int, innode: Int, outnode: Int, splittedLinkInnovationNumber: Int) extends NodeInnovation with Innovation

//}

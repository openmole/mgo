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

/**
 * TODO: Définir ConnectionGene[+I ...] pour que postBreeding puisse retourner Genome[NumberedInnovation], des génomes dont tous les genes sont numérotés.
 * Ça m'évitera de devoir chequer le type d'innovation à chaque fois que je veux acceder à son number (voir archive)*
 */

package fr.iscpif.mgo.genome

import scala.util.Random
import monocle.Lens
import fr.iscpif.mgo._

/**
 * Genome for NEAT
 */
trait NEATGenome <: GeneticBreeding {
  type G = NEATGenome.Genome[NEATGenome.Innovation]

  def inputNodes: Int
  def outputNodes: Int

  lazy val minimalGenome: G = ??? //check innovation number?
  // NEATGenome.Genome(
  //   (for {
  //     u <- 1 to inputNodes
  //     v <- inputNodes + 1 to inputNodes + outputNodes
  //   } yield NEATGenome.ConnectionGene(u, v, 0, true, None)) toVector,
  //   ((1 to inputNodes).toIterator.map { NEATGenome.InputNode(_) })
  //     ++ ((inputNodes + 1 to inputNodes + outputNodes).toIterator.map { NEATGenome.OutputNode(_) }) toArray)

}

object NEATGenome {
  case class Genome[+I <: Innovation](
      connectionGenes: Seq[ConnectionGene[I]],
      nodes: Seq[Int]) {
    def setInnovationNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[NumberedInnovation]): (Genome[NumberedInnovation], Int, Seq[NumberedInnovation]) = {
      val (newconnectiongenes, newgin, newroi) =
        connectionGenes.foldLeft((Seq[ConnectionGene[NumberedInnovation]](), globalInnovationNumber, recordOfInnovations)) { (acc, cg) =>
          val (curcgs, curgin, curroi) = acc
          val (newcg, newgin, newroi) = cg.setInnovationNumber(curgin, curroi)
          (curcgs :+ newcg, newgin, newroi)
        }
      (copy(connectionGenes = newconnectiongenes), newgin, newroi)
    }
  }

  case class ConnectionGene[+I <: Innovation](
      inNode: Int,
      outNode: Int,
      weight: Double,
      enabled: Boolean,
      innovation: I,
      species: Int) {
    def setInnovationNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[NumberedInnovation]): (ConnectionGene[NumberedInnovation], Int, Seq[NumberedInnovation]) = {
      val (newinnov, newgin, newroi) = innovation.setNumber(globalInnovationNumber, recordOfInnovations)
      (copy(innovation = newinnov), newgin, newroi)
    }

    //def setInnovationNumber(x: Int): ConnectionGene[NumberedInnovation] = copy(innovation = innovation.setNumber(x))
  }

  // val TTT: ConnectionGene[Innovation] = (ConnectionGene(1, 2, 3.0, true, NumberedLinkInnovation(1, 2, 3), 0): ConnectionGene[NumberedInnovation])
  // val GGG: Genome[Innovation] = 

  sealed trait Innovation {
    def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[NumberedInnovation]): (NumberedInnovation, Int, Seq[NumberedInnovation])
    def setNumber(x: Int): NumberedInnovation
    def unsetNumber: UnnumberedInnovation
    def sameAs(x: Innovation): Boolean
  }
  sealed trait UnnumberedInnovation extends Innovation {
    def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[NumberedInnovation]): (NumberedInnovation, Int, Seq[NumberedInnovation]) =
      recordOfInnovations.find { sameAs(_) } match {
        case Some(oldinnov) => (setNumber(oldinnov.number), globalInnovationNumber, recordOfInnovations)
        case None => {
          val innovWithNumber = setNumber(globalInnovationNumber + 1)
          (innovWithNumber, globalInnovationNumber + 1, recordOfInnovations :+ innovWithNumber)
        }
      }
  }
  sealed trait NumberedInnovation extends Innovation {
    val number: Int
    def setNumber(globalInnovationNumber: Int, recordOfInnovations: Seq[NumberedInnovation]): (NumberedInnovation, Int, Seq[NumberedInnovation]) =
      (this, globalInnovationNumber, recordOfInnovations)
  }
  sealed trait LinkInnovation extends Innovation {
    val innode: Int
    val outnode: Int
    def setNumber(x: Int): NumberedLinkInnovation = NumberedLinkInnovation(x, innode, outnode)
    def unsetNumber: UnnumberedLinkInnovation = UnnumberedLinkInnovation(innode, outnode)
    def sameAs(x: Innovation): Boolean =
      x match {
        case x: LinkInnovation => (innode == x.innode) && (outnode == x.outnode)
        case _ => false
      }
  }
  sealed trait NodeInnovation extends Innovation {
    val innode: Int
    val outnode: Int
    val splittedLinkInnovationNumber: Int
    def setNumber(x: Int): NumberedNodeInnovation = NumberedNodeInnovation(x, innode, outnode, splittedLinkInnovationNumber)
    def unsetNumber: UnnumberedNodeInnovation = UnnumberedNodeInnovation(innode, outnode, splittedLinkInnovationNumber)
    def sameAs(x: Innovation): Boolean =
      x match {
        case x: NodeInnovation => (innode == x.innode) && (outnode == x.outnode) && (splittedLinkInnovationNumber == x.splittedLinkInnovationNumber)
        case _ => false
      }
  }
  case class UnnumberedLinkInnovation(innode: Int, outnode: Int) extends LinkInnovation with UnnumberedInnovation
  case class NumberedLinkInnovation(number: Int, innode: Int, outnode: Int) extends LinkInnovation with NumberedInnovation
  case class UnnumberedNodeInnovation(innode: Int, outnode: Int, splittedLinkInnovationNumber: Int) extends NodeInnovation with UnnumberedInnovation
  case class NumberedNodeInnovation(number: Int, innode: Int, outnode: Int, splittedLinkInnovationNumber: Int) extends NodeInnovation with NumberedInnovation

  sealed abstract class Node(id: Int)
  case class InputNode(id: Int) extends Node(id)
  case class OutputNode(id: Int) extends Node(id)
  case class HiddenNode(id: Int) extends Node(id)
  case class BiasNode(id: Int) extends Node(id)

}

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
import monocle._

object NEATGenome {
  case class ConnectionGene(
    inNode: Int,
    outNode: Int,
    weight: Double,
    enabled: Boolean,
    innovationNumber: Int)
  abstract class Node(id: Int)
  case class InputNode(id: Int) extends Node(id)
  case class OutputNode(id: Int) extends Node(id)
  case class HiddenNode(id: Int) extends Node(id)
  case class BiasNode(id: Int) extends Node(id)
  case class Genome(
    connectionGenes: Array[ConnectionGene],
    nodes: Array[Node])

}

/**
 * Genome for NEAT
 */
trait NEATGenome {
  type G = NEATGenome.Genome

  def inputNodes: Int
  def outputNodes: Int

  lazy val minimalGenome: G =
    NEATGenome.Genome(
      (for {
        u <- 1 to inputNodes
        v <- inputNodes + 1 to inputNodes + outputNodes
      } yield NEATGenome.ConnectionGene(u, v, 0, true, 0)) toArray,
      ((1 to inputNodes).toIterator.map { NEATGenome.InputNode(_) })
        ++ ((inputNodes + 1 to inputNodes + outputNodes).toIterator.map { NEATGenome.OutputNode(_) }) toArray)
}


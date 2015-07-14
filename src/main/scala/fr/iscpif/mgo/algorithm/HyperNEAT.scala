/*
 * Copyright (C) 08/07/2015 Guillaume Chérel
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

package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import fr.iscpif.mgo.genome.MinimalGenome
import fr.iscpif.mgo.problem.NEATProblem
import fr.iscpif.mgo.tools.neuralnetwork.ActivationFunction

import util.Random

/**
 * Differences with NEAT:
 * - Nodes carry activation functions
 * - Neural nets are created from the evolved nets
 */
trait HyperNEAT <: NEATProblem with NEATBreeding with NEATElitism with NEATArchive with NoPhenotype with MinimalGenome {

  type ACTIVF = String
  val cppnActivationFunctions: Seq[ACTIVF]

  type NODEDATA = ACTIVF

  def pickActivationFunction(implicit rng: Random): ACTIVF = cppnActivationFunctions(rng.nextInt(cppnActivationFunctions.length))

  override def pickNewHiddenNode(level: Double)(implicit rng: Random): Node =
    HiddenNode(
      pickActivationFunction,
      level)

  def newInputNode: InputNode = InputNode("lin")
  def newBiasNode: BiasNode = BiasNode("lin")
  def newOutputNode: OutputNode = OutputNode("lin")
}

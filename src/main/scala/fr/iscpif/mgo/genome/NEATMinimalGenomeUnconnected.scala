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

package fr.iscpif.mgo.genome

import scala.util.Random
import collection.immutable.IntMap

trait NEATMinimalGenomeUnconnected <: MinimalGenome with NEATGenome {
  lazy val minimalGenome: G =
    NEATGenome.Genome(
      connectionGenes = Seq[NEATGenome.ConnectionGene](),
      nodes =
        IntMap(
          inputNodesIndices.map { _ -> NEATGenome.InputNode() }
            ++ biasNodesIndices.map { _ -> NEATGenome.BiasNode() }
            ++ outputNodesIndices.map { _ -> NEATGenome.OutputNode() }.toSeq: _*),
      species = 0,
      lastNodeId = inputNodes + biasNodes + outputNodes - 1
    )

}

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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import util.Random
import fr.iscpif.mgo.genome.RandomGenome
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.genome.NEATGenome._
import fr.iscpif.mgo.archive.NEATArchive
import collection.immutable.IntMap

trait NEATAnyTopolgy extends NEATBreeding with NEATGenome {

  def mutateAddLink(
    genome: NEATGenome.Genome[NEATGenome.NumberedInnovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): NEATGenome.Genome[NEATGenome.Innovation] = {
    // allow destination nodes to be input nodes?
    // allow recursion?
    // look for nodes that are not already connected
    val connections =
      IntMap[Seq[Int]](
        genome.connectionGenes.map { cg => (cg.inNode -> cg.outNode) }
          .groupBy { (_: (Int, Int))._1 }
          .mapValues { (_: Seq[(Int, Int)]).map { _._2 } }.toSeq: _*)

    val pair: Option[(Int, Int)] =
      (if (rng.nextDouble() < mutationAddLinkBiasProb)
        rng.shuffle(biasNodesIndices.iterator)
      else
        rng.shuffle(genome.nodes.keysIterator))
        .flatMap { u => rng.shuffle(genome.nodes.keysIterator).map { v => (u, v) } }
        .find {
          case (u: Int, v: Int) =>
            !connections(u).contains(v)
        }

    pair match {
      case None => genome
      case Some((u, v)) => {
        val newgene =
          NEATGenome.ConnectionGene(
            inNode = u,
            outNode = v,
            weight = initialWeight,
            enabled = true,
            innovation = NEATGenome.UnnumberedLinkInnovation(u, v))
        NEATGenome.Genome[NEATGenome.Innovation](
          connectionGenes =
            genome.connectionGenes :+ newgene,
          nodes = genome.nodes,
          species = genome.species,
          lastNodeId = genome.lastNodeId)
      }
    }
  }
}
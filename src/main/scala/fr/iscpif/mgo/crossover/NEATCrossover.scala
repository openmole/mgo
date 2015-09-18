/*
 * Copyright (C) 17/09/2015 Guillaume Chérel
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
package fr.iscpif.mgo.crossover

import fr.iscpif.mgo.fitness.DoubleFitness
import fr.iscpif.mgo.{ Individual, Population }
import fr.iscpif.mgo.genome.NEATGenomesAlign
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.genome.NEATGenome.Genome

import scalaz._
import Scalaz._

import scala.collection.immutable.IntMap
import scala.util.Random

trait NEATCrossover <: Crossover with NEATGenomesAlign with NEATGenome with DoubleFitness {

  def crossoverInheritDisabledProb: Double

  override def crossover(indivs: Seq[Individual[G, P, F]], population: Population[G, P, F], archive: A)(implicit rng: Random): BreedingContext[Vector[G]] = {
    val Vector(i1, i2) = indivs

    // - genes that match no other gene in the other genome are disjoint if they occur within the range of the the other genome innovation numbers
    // - they are excess if they occur outside this range.
    val aligned = alignGenomes(i1.genome.connectionGenes, i2.genome.connectionGenes)

    // - a new offspring is composed by chosing each matching genes randomly from either parent.
    // - excess or disjoint genes from the fittest parent are included.
    // - if the parents fitnesses are the same, inherit from the smallest genome or randomly
    val fittest = if (i1.fitness == i2.fitness) 0 else if (i1.fitness > i2.fitness) 1 else 2

    val newconnectiongenes =
      aligned.foldLeft(Seq[ConnectionGene]()) { (acc, pair) =>
        pair match {
          case (None, Some(cg2), _) =>
            if (fittest == 1) acc
            else if (fittest == 2) acc :+ cg2
            else if (rng.nextBoolean()) acc else acc :+ cg2

          case (Some(cg1), None, _) =>
            if (fittest == 1) acc :+ cg1
            else if (fittest == 2) acc
            else if (rng.nextBoolean()) acc :+ cg1 else acc

          case (Some(cg1), Some(cg2), _) =>
            val inherit =
              if (fittest == 1) cg1
              else if (fittest == 2) cg2
              else if (rng.nextBoolean()) cg1 else cg2

            // If one gene is disabled, the offspring gene is more likely to be disabled
            val disable =
              if (!(cg1.enabled && cg2.enabled)) rng.nextDouble() < crossoverInheritDisabledProb
              else false

            if (disable) acc :+ inherit.copy(enabled = false)
            else acc :+ inherit

          case (None, None, _) => acc
        }
      }.toVector

    // Include at least one node from each parent. Also include one for each input, output, and bias node that are not connected.
    val newnodesidx =
      (newconnectiongenes.flatMap { cg => Seq(cg.inNode, cg.outNode) } ++
        (inputNodesIndices ++ biasNodesIndices ++ outputNodesIndices)
      ).toSet
    val newnodes = IntMap(newnodesidx.toSeq.map { i =>
      i -> {
        if (!i1.genome.nodes.contains(i)) i2.genome.nodes(i)
        else if (!i2.genome.nodes.contains(i)) i1.genome.nodes(i)
        else if (rng.nextBoolean()) i1.genome.nodes(i) else i2.genome.nodes(i)
      }
    }: _*)

    if (newconnectiongenes.exists(cg1 => {
      if (newnodes(cg1.inNode).level >= newnodes(cg1.outNode).level) {
        println(cg1)
        true
      } else false
    })) throw new RuntimeException(s"Backwards!! $newconnectiongenes $newnodes\n$i1\n$i2")
    val result = Genome(
      connectionGenes = newconnectiongenes,
      nodes = newnodes,
      // Give the offspring its parents species. Species will be reattributed at the postBreeding
      // stage anyway. This will just be used as a hint to speed up species attribution
      species = i1.genome.species,
      lastNodeId = newnodes.lastKey)

    Vector(result).point[BreedingContext]
  }
}


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

//TODO: mutate links (enabled), mutate nodes

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import util.Random
import fr.iscpif.mgo.genome.RandomGenome
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.genome.NEATGenome._
import fr.iscpif.mgo.archive.NEATArchive
import collection.immutable.IntMap

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait NEATBreeding <: StatefulGeneticBreeding with NEATArchive with NEATGenome with NEATNetworkTopology {

  type IG = Genome[Innovation]
  type BreedingState = Unit

  def mutationAddNodeProb = 0.2
  def mutationAddLinkProb = 0.2
  require(mutationAddNodeProb + mutationAddLinkProb <= 1)

  def mutationWeightSigma = 1
  def mutationWeightProb0 = 0.5
  def mutationDisableProb = 0.2
  def mutationEnableProb = 0.2

  def crossoverInheritDisabledProb = 0.75

  def initialWeight: Double

  def initialGenome(s: BreedingState): IG = minimalGenome

  def initialBreedingState(population: Population[G, P, F], archive: A): BreedingState = ()

  /**Select an individual among the population.*/
  def selection(
    population: Population[G, P, F],
    archive: A,
    s: BreedingState)(implicit rng: Random): Iterator[Individual[G, P, F]] = ???

  def crossover(
    i1: Individual[Genome[NumberedInnovation], P, Double], //Genome[NumberedInnovation],
    i2: Individual[Genome[NumberedInnovation], P, Double], //Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A,
    s: BreedingState)(implicit rng: Random): Genome[NumberedInnovation] = {
    // - align the 2 parent genomes according to their innovation number
    // - genes that match no other gene in the other genome are disjoint if they occur within the range of the the other genome innovation numbers
    // - they are excess if they occur outside this range.
    val aligned = alignGenomes(i1.genome.connectionGenes, i2.genome.connectionGenes)
    // - a new offspring is composed by chosing each matching genes randomly from either parent.
    // - excess or disjoint genes from the fittest parent are included.
    // - if the parents fitnesses are the same, inherit from the smallest genome or randomly
    val fittest = if (i1.fitness == i2.fitness) 0 else if (i1.fitness > i2.fitness) 1 else 2
    val newconnectiongenes =
      aligned.foldLeft(Seq[ConnectionGene[NumberedInnovation]]()) { (acc, pair) =>
        pair match {
          case (None, Some(cg2)) =>
            if (fittest == 1) acc
            else if (fittest == 2) acc :+ cg2
            else if (rng.nextBoolean()) acc else acc :+ cg2
          case (Some(cg1), None) =>
            if (fittest == 1) acc :+ cg1
            else if (fittest == 2) acc
            else if (rng.nextBoolean()) acc :+ cg1 else acc
          case (Some(cg1), Some(cg2)) =>
            if (fittest == 1) acc :+ cg1
            else if (fittest == 2) acc :+ cg2
            else if (rng.nextBoolean()) acc :+ cg1 else acc :+ cg2
          case (None, None) => acc
        }
      }
    // For all nodes in the connection genes, include one randomly from each parent.
    val newnodesidx = newconnectiongenes.flatMap { cg => Seq(cg.inNode, cg.outNode) }.toSet
    val newnodes = IntMap(newnodesidx.toSeq.map { i =>
      (i ->
        (if (rng.nextBoolean()) i1.genome.nodes(i) else i2.genome.nodes(i)))
    }: _*)
    Genome[NumberedInnovation](
      connectionGenes = newconnectiongenes,
      nodes = newnodes,
      species = None)
  }

  def alignGenomes(
    cg1: Seq[ConnectionGene[NumberedInnovation]],
    cg2: Seq[ConnectionGene[NumberedInnovation]]): List[(Option[ConnectionGene[NumberedInnovation]], Option[ConnectionGene[NumberedInnovation]])] = {
    if (cg1.isEmpty && cg2.isEmpty)
      List.empty
    else if (cg1.isEmpty)
      (None, Some(cg2.head)) :: alignGenomes(Seq.empty, cg2.tail)
    else if (cg2.isEmpty)
      (Some(cg1.head), None) :: alignGenomes(cg1.tail, Seq.empty)
    else if (cg1.head.innovation.number == cg2.head.innovation.number)
      (Some(cg1.head), Some(cg2.head)) :: alignGenomes(cg1.tail, cg2.tail)
    else if (cg1.head.innovation.number < cg2.head.innovation.number)
      (Some(cg1.head), None) :: alignGenomes(cg1.tail, cg2)
    else
      (None, Some(cg2.head)) :: alignGenomes(cg1, cg2.tail)
  }

  def mutate(
    genome: Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A,
    s: BreedingState)(implicit rng: Random): Genome[Innovation] = {
    val r = rng.nextDouble
    if (r < mutationAddNodeProb) {
      mutateAddNode(genome, population, archive, s)
    } else if (r < mutationAddNodeProb + mutationAddLinkProb) {
      mutateAddLink(genome, population, archive, s)
    } else
      mutateWeights(genome, population, archive, s)
  }

  def mutateAddNode(
    genome: Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A,
    s: BreedingState)(implicit rng: Random): Genome[Innovation] = {
    //pick a connection gene, disable it and add 2 new connection genes
    val picked: Int = rng.nextInt(genome.connectionGenes.length)
    val pickedcg = genome.connectionGenes(picked)
    val newnodeid: Int = genome.nodes.size
    val newcg1: ConnectionGene[UnnumberedInnovation] =
      ConnectionGene(
        inNode = pickedcg.inNode,
        outNode = newnodeid,
        weight = 1,
        enabled = true,
        innovation = UnnumberedNodeInnovation(pickedcg.inNode, pickedcg.outNode, pickedcg.innovation.number))
    val newcg2: ConnectionGene[UnnumberedInnovation] =
      ConnectionGene(
        inNode = newnodeid,
        outNode = pickedcg.outNode,
        weight = pickedcg.weight,
        enabled = true,
        innovation = UnnumberedNodeInnovation(pickedcg.inNode, pickedcg.outNode, pickedcg.innovation.number))
    Genome[Innovation](
      connectionGenes =
        (genome.connectionGenes.updated(
          picked,
          pickedcg.copy(enabled = false)
        ) ++ Vector(newcg1, newcg2)),
      nodes = genome.nodes + (newnodeid -> HiddenNode(level = (genome.nodes(pickedcg.inNode).level + genome.nodes(pickedcg.outNode).level) / 2.0)),
      species = None)
  }

  def mutateWeights(
    genome: Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A,
    s: BreedingState)(implicit rng: Random): Genome[Innovation] =
    genome.copy(
      connectionGenes =
        genome.connectionGenes.map { (cg: ConnectionGene[Innovation]) =>
          cg.copy(weight = mutateWeight(cg.weight))
        })

  def mutateWeight(x: Double)(implicit rng: Random): Double =
    if (rng.nextDouble < mutationWeightProb0) 0
    else x + rng.nextGaussian() * mutationWeightSigma

  def updatedState(s: BreedingState, newoffspring: IG): BreedingState = ()

  /**
   * Sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found it the archive's record of innovations are attributed its number
   */
  def postBreeding(
    population: Population[Genome[NumberedInnovation], P, F],
    offsprings: Seq[Genome[Innovation]],
    archive: NEATArchive.Archive,
    s: BreedingState)(implicit rng: Random): Seq[Genome[NumberedInnovation]] = {
    // Set innovation numbers in all genomes
    val (newgenomesWNumbers, newgin, newroi) = offsprings.foldLeft((Seq[Genome[NumberedInnovation]](), archive.globalInnovationNumber, archive.recordOfInnovations)) { (acc, genome) =>
      val (curgenomes, curgin, curroi) = acc
      val (newgenome, newgin, newroi) = genome.setInnovationNumber(curgin, curroi)
      (curgenomes :+ newgenome, newgin, newroi)
    }
    setSpecies(newgenomesWNumbers)
  }

}

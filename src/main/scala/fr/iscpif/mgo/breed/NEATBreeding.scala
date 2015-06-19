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
import collection.immutable.Map
import math.{ max, abs, round }

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */
trait NEATBreeding <: Breeding with NEATArchive with NEATGenome with NEATNetworkTopology with Lambda with DoubleFitness with MinimalGenome {

  def mutationAddNodeProb: Double // = 0.2
  def mutationAddLinkProb: Double // = 0.2
  require(mutationAddNodeProb + mutationAddLinkProb <= 1)

  def mutationAddLinkBiasProb: Double

  def mutationWeightSigma: Double // = 1
  def mutationWeightProb0: Double // = 0.5
  def mutationDisableProb: Double // = 0.2
  def mutationEnableProb: Double // = 0.2

  def genDistDisjointCoeff: Double // = 1.0
  def genDistExcessCoeff: Double // = 1.0
  def genDistWeightDiffCoeff: Double // = 0.4

  def speciesCompatibilityThreshold: Double // = 3.0

  def crossoverInheritDisabledProb: Double // = 0.75

  def initialWeight(implicit rng: Random): Double = rng.nextGaussian() * mutationWeightSigma

  def initialGenome: Genome[NumberedInnovation] = minimalGenome

  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Seq[G] = {

    if (population.isEmpty) Seq(initialGenome)
    else {

      val parents = selection(population, archive, size)

      val breeding: Iterator[Genome[Innovation]] =
        parents.map {
          case (i1, i2) =>
            mutate(
              crossover(i1, i2, population, archive),
              population,
              archive)
        }

      val offsprings =
        if (population.isEmpty) Seq(initialGenome)
        else breeding.toSeq

      postBreeding(offsprings, population, archive)
    }
  }

  /**Select an individual among the population.*/
  def selection(
    population: Population[G, P, F],
    archive: A,
    size: Int)(implicit rng: Random): Iterator[(Individual[G, P, F], Individual[G, P, F])] = {
    val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
      population.toIndividuals.groupBy { indiv => indiv.genome.species }
    val speciesFitnesses: Seq[(Int, Double)] = indivsBySpecies.iterator.map { case (sp, indivs) => (sp, indivs.map { _.fitness }.sum / indivs.size) }.toSeq
    val sumOfSpeciesFitnesses: Double = speciesFitnesses.map { _._2 }.sum
    val speciesOffsprings: Seq[(Int, Int)] = speciesFitnesses.map { case (sp, f) => (sp, round(f * size / sumOfSpeciesFitnesses).toInt) }
    speciesOffsprings.flatMap {
      case (species, nb) =>
        Iterator.fill(nb) {
          val nparents = indivsBySpecies(species).length
          val p1 = rng.nextInt(nparents)
          val p2 = rng.nextInt(nparents)
          (indivsBySpecies(species)(p1), indivsBySpecies(species)(p2))
        }
    }.toIterator
  }

  def crossover(
    i1: Individual[Genome[NumberedInnovation], P, Double], //Genome[NumberedInnovation],
    i2: Individual[Genome[NumberedInnovation], P, Double], //Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): Genome[NumberedInnovation] = {
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
          case (None, Some(cg2), _) =>
            if (fittest == 1) acc
            else if (fittest == 2) acc :+ cg2
            else if (rng.nextBoolean()) acc else acc :+ cg2

          case (Some(cg1), None, _) =>
            if (fittest == 1) acc :+ cg1
            else if (fittest == 2) acc
            else if (rng.nextBoolean()) acc :+ cg1 else acc

          case (Some(cg1), Some(cg2), _) => {
            val inherit =
              if (fittest == 1) cg1
              else if (fittest == 2) cg2
              else if (rng.nextBoolean()) cg1 else cg2

            // If one gene is disabled, the offspring gene is more likely to be disabled
            val disable =
              if (!(cg1.enabled && cg2.enabled)) (rng.nextDouble() < crossoverInheritDisabledProb)
              else false

            if (disable) acc :+ inherit.copy(enabled = false)
            else acc :+ inherit
          }

          case (None, None, _) => acc
        }
      }
    // For all nodes in the connection genes, include one randomly from each parent. Also include one for each input, output, and bias node that are not connected.
    val newnodesidx =
      (newconnectiongenes.flatMap { cg => Seq(cg.inNode, cg.outNode) } ++
        (inputNodesIndices ++ biasNodesIndices ++ outputNodesIndices)
      ).toSet
    val newnodes = IntMap(newnodesidx.toSeq.map { i =>
      (i ->
        {
          if (!i1.genome.nodes.contains(i)) i2.genome.nodes(i)
          else if (!i2.genome.nodes.contains(i)) i1.genome.nodes(i)
          else if (rng.nextBoolean()) i1.genome.nodes(i) else i2.genome.nodes(i)
        })
    }: _*)
    Genome[NumberedInnovation](
      connectionGenes = newconnectiongenes,
      nodes = newnodes,
      // Give the offspring its parents species. Species will be reattributed at the postBreeding 
      // stage anyway. This will just be used as a hint to speed up species attribution
      species = i1.genome.species)
  }

  object AlignmentInfo extends Enumeration {
    type AlignmentInfo = Value
    val Aligned, Excess, Disjoint = Value
  }
  import AlignmentInfo._
  /**
   * Returns a list of aligned connection genes. The first two elements of each tuple give aligned genes, (or None for unmatching genes) and the third is set to 0 when the genes are
   * aligned, 1 for an excess genes, and 2 for disjoint genes
   */
  def alignGenomes(
    cg1: Seq[ConnectionGene[NumberedInnovation]],
    cg2: Seq[ConnectionGene[NumberedInnovation]]): List[(Option[ConnectionGene[NumberedInnovation]], Option[ConnectionGene[NumberedInnovation]], AlignmentInfo)] = {
    if (cg1.isEmpty && cg2.isEmpty)
      List.empty
    else if (cg1.isEmpty)
      (None, Some(cg2.head), Disjoint) :: alignGenomes(Seq.empty, cg2.tail)
    else if (cg2.isEmpty)
      (Some(cg1.head), None, Disjoint) :: alignGenomes(cg1.tail, Seq.empty)
    else if (cg1.head.innovation.number == cg2.head.innovation.number)
      (Some(cg1.head), Some(cg2.head), Aligned) :: alignGenomes(cg1.tail, cg2.tail)
    else if (cg1.head.innovation.number < cg2.head.innovation.number)
      (Some(cg1.head), None, Excess) :: alignGenomes(cg1.tail, cg2)
    else
      (None, Some(cg2.head), Excess) :: alignGenomes(cg1, cg2.tail)
  }

  def mutate(
    genome: Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): Genome[Innovation] = {
    val r = rng.nextDouble
    if (r < mutationAddNodeProb) {
      mutateAddNode(genome, population, archive)
    } else if (r < mutationAddNodeProb + mutationAddLinkProb) {
      mutateAddLink(genome, population, archive)
    } else
      // successively mutate weights and mutate enable bits on the genome
      { mutateWeights(_: Genome[Innovation], population, archive) }.andThen {
        mutateEnabled(_: Genome[Innovation], population, archive)
      }(genome)
  }

  def mutateAddNode(
    genome: Genome[NumberedInnovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): Genome[Innovation] = {
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
      species = genome.species)
  }

  def mutateWeights(
    genome: Genome[Innovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): Genome[Innovation] =
    genome.copy(
      connectionGenes =
        genome.connectionGenes.map { (cg: ConnectionGene[Innovation]) =>
          cg.copy(weight = mutateWeight(cg.weight))
        })

  def mutateWeight(x: Double)(implicit rng: Random): Double =
    if (rng.nextDouble < mutationWeightProb0) 0
    else x + rng.nextGaussian() * mutationWeightSigma

  def mutateEnabled(
    g: Genome[Innovation],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): Genome[Innovation] =
    g.copy(
      connectionGenes =
        g.connectionGenes.map { mutateEnabled(_) })

  def mutateEnabled(cg: ConnectionGene[Innovation])(implicit rng: Random): ConnectionGene[Innovation] = {
    val newenabled =
      if (cg.enabled) (if (rng.nextDouble() < mutationDisableProb) !cg.enabled else cg.enabled)
      else (if (rng.nextDouble() < mutationEnableProb) !cg.enabled else cg.enabled)
    cg.copy(enabled = newenabled)
  }

  /**
   * Sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found it the archive's record of innovations are attributed its number
   */
  def postBreeding(
    offsprings: Seq[Genome[Innovation]],
    population: Population[Genome[NumberedInnovation], P, F],
    archive: NEATArchive.Archive)(implicit rng: Random): Seq[Genome[NumberedInnovation]] = {
    // Set innovation numbers in all offsprings, and then Attribute a species to each offspring
    { setInnovationNumbers(_: Seq[Genome[Innovation]], archive) }.andThen {
      setSpecies(_: Seq[Genome[NumberedInnovation]], archive)
    }(offsprings)
  }

  def setInnovationNumbers(
    offsprings: Seq[Genome[Innovation]],
    archive: NEATArchive.Archive): Seq[Genome[NumberedInnovation]] = {
    val (newgenomes, newgin, newroi) = offsprings.foldLeft((Seq[Genome[NumberedInnovation]](), archive.globalInnovationNumber, archive.recordOfInnovations)) { (acc, genome) =>
      val (curgenomes, curgin, curroi) = acc
      val (newgenome, newgin, newroi) = genome.setInnovationNumber(curgin, curroi)
      (curgenomes :+ newgenome, newgin, newroi)
    }
    newgenomes
  }

  def setSpecies(
    offsprings: Seq[Genome[NumberedInnovation]],
    archive: NEATArchive.Archive): Seq[Genome[NumberedInnovation]] = {
    val (newgenomes, _) = offsprings.foldLeft((Seq[Genome[NumberedInnovation]](), archive.indexOfSpecies)) { (acc, genome) =>
      val (curgenomes, curios) = acc
      // Find a genome in the index of species which is sufficiently similar to the current genome. 
      // Start by checking if the offspring is compatible with the parent species (currently set in the species member of the offspring)
      if (genomesDistance(genome, curios(genome.species)) < speciesCompatibilityThreshold)
        (curgenomes :+ genome, curios)
      else
        curios.find {
          case (sindex, prototype) => genomesDistance(prototype, genome) < speciesCompatibilityThreshold
        } match {
          //If found, attribute the species to the current genome
          case Some((species, _)) =>
            (curgenomes :+ genome.copy(species = species),
              curios)
          //Otherwise, create a new species with the current genome as the prototype
          case None => {
            val newspecies = curios.lastKey + 1
            val newgenome = genome.copy(species = newspecies)
            (curgenomes :+ newgenome,
              curios + (newspecies, newgenome))
          }
        }
    }
    newgenomes
  }

  def genomesDistance(
    g1: Genome[NumberedInnovation],
    g2: Genome[NumberedInnovation]): Double = {
    val alignment = alignGenomes(g1.connectionGenes, g2.connectionGenes)
    val (excess, disjoint, weightdiff, matchingGenes) = distanceFactors(alignment)
    val longestGenomeSize = max(g1.connectionGenes.length, g2.connectionGenes.length)
    (genDistDisjointCoeff * excess / longestGenomeSize) +
      (genDistExcessCoeff * disjoint / longestGenomeSize) +
      (genDistWeightDiffCoeff * weightdiff / matchingGenes)
  }

  def distanceFactors(alignment: List[(Option[ConnectionGene[NumberedInnovation]], Option[ConnectionGene[NumberedInnovation]], AlignmentInfo)]): (Int, Int, Double, Int) =
    alignment match {
      case List() => (0, 0, 0, 0)
      case head :: tail => {
        val (excess, disjoint, weightdiff, matchingGenes) = distanceFactors(tail)
        head match {
          case (Some(cg1), Some(cg2), Aligned) => (excess, disjoint, weightdiff + abs(cg1.weight - cg2.weight), matchingGenes + 1)
          case (_, _, Disjoint) => (excess, disjoint + 1, weightdiff, matchingGenes)
          case (_, _, Excess) => (excess + 1, disjoint, weightdiff, matchingGenes)
        }
      }
    }

}

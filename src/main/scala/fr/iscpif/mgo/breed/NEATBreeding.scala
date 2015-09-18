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
import scala.annotation.tailrec
import util.Random
import fr.iscpif.mgo.tools.StateMonad
import fr.iscpif.mgo.genome.NEATGenome
import fr.iscpif.mgo.genome.NEATGenome._
import fr.iscpif.mgo.archive.NEATArchive
import collection.immutable.IntMap
import collection.immutable.Map
import math.{ max, min, abs, round }

/**
 * Layer of the cake for the breeding part of the evolution algorithm
 */

trait NEATBreeding {}

/*trait NEATBreeding <: Breeding with NEATArchive with NEATGenome with Lambda with DoubleFitness with MinimalGenome {

  type BreedingState = (Int, //global innovation number
  Int, //global node number
  Seq[Innovation], //record of innovations
  IntMap[G]) //index of species

  def interSpeciesMatingProb: Double

  def mutationAddNodeProb: Double

  // = 0.2
  def mutationAddLinkProb: Double // = 0.2
  require(mutationAddNodeProb + mutationAddLinkProb <= 1)

  def mutationAddLinkBiasProb: Double

  def mutationWeightSigma: Double

  // = 1
  def mutationWeightProb0: Double

  def mutationWeightDriftTo0: Double
  def mutationWeightHardMax: Double
  def mutationWeightHardMin: Double

  // = 0.5
  def mutationDisableProb: Double

  // = 0.2
  def mutationEnableProb: Double // = 0.2

  def genDistDisjointCoeff: Double

  // = 1.0
  def genDistExcessCoeff: Double

  // = 1.0
  def genDistWeightDiffCoeff: Double // = 0.4

  def crossoverInheritDisabledProb: Double // = 0.75

  def initialWeight(implicit rng: Random): Double = rng.nextGaussian() * mutationWeightSigma

  def useSpeciesHint: Boolean

  def breed(population: Population[G, P, F], archive: A, size: Int)(implicit rng: Random): Seq[G] = {

    val (globalInnovationNumber, globalNodeNumber) = population.foldLeft((0, 0)) {
      case (acc, indiv) =>
        val (gin, gnn) = acc
        val newgin = if (indiv.genome.connectionGenes.isEmpty) 0 else max(gin, indiv.genome.connectionGenes.map {
          _.innovation
        }.max)
        val newgnn = max(gnn, indiv.genome.lastNodeId)
        (newgin, newgnn)
    }

    if (population.isEmpty) Vector.fill(size)(minimalGenome)
    else {
      val parents = selection(population, archive, size).toList

      val (p1, p2) = parents.head
      val breedAll: StateMonad[List[G], BreedingState] =
        parents.tail.foldLeft(doBreed(p1, p2, population, archive)(List.empty)) {
          (acc, parents) =>
            val (p1, p2) = parents
            acc bind doBreed(p1, p2, population, archive)
        }

      val (bred, _) = breedAll.runstate((globalInnovationNumber, globalNodeNumber, Seq.empty, archive.indexOfSpecies))
      bred.toSeq

    }
  }

  def doWithParents(p1: Individual[G, P, F], p2: Individual[G, P, F]): StateMonad[(Individual[G, P, F], Individual[G, P, F]), BreedingState] =
    StateMonad.pure((p1, p2))

  def doCrossover(
    parents: (Individual[G, P, F], Individual[G, P, F]),
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): StateMonad[G, BreedingState] =
    StateMonad[G, BreedingState]({
      state => //this crossover doesn't actually use state
        val (gin, gnn, roi, ios) = state
        val crossed = crossover(parents._1, parents._2)
        (crossed, (gin, gnn, roi, ios))
    })

  def doMutate(
    g: G,
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): StateMonad[G, BreedingState] =
    StateMonad[G, BreedingState]({
      state =>
        mutate(g, state)
    })

  def doPostBreeding(
    g: G,
    population: Population[G, P, F],
    archive: A): StateMonad[G, BreedingState] =
    StateMonad[G, BreedingState]({
      case (gin, gnn, roi, ios) =>
        val (newGenome, newios) = setSpecies(g, ios, archive.speciesCompatibilityThreshold.head)
        //println(s"indiv attributed to species ${newGenome.species}")
        (newGenome, (gin, gnn, roi, newios))
    })

  def doBreedOnce(
    p1: Individual[G, P, F],
    p2: Individual[G, P, F],
    population: Population[G, P, F],
    archive: A)(implicit rng: Random): StateMonad[G, BreedingState] =
    doWithParents(p1, p2).bind {
      doCrossover(_, population, archive)
    }.bind {
      doMutate(_, population, archive)
    }.bind {
      doPostBreeding(_, population, archive)
    }

  def doBreed(
    p1: Individual[G, P, F],
    p2: Individual[G, P, F],
    population: Population[G, P, F],
    archive: A)(
      curoffsprings: List[G])(implicit rng: Random): StateMonad[List[G], BreedingState] = {
    StateMonad({
      state =>
        val (thisoffspring, newstate) = doBreedOnce(p1, p2, population, archive).runstate(state)
        (thisoffspring :: curoffsprings, newstate)
    })
  }

  /** Select an individual among the population. */
  def selection(
    population: Population[G, P, F],
    archive: A,
    size: Int)(implicit rng: Random): Iterator[(Individual[G, P, F], Individual[G, P, F])] = {
    val indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]] =
      population.toIndividuals.groupBy { indiv => indiv.genome.species }
    speciesOffsprings(indivsBySpecies, size).flatMap {
      case (species, nb) =>
        Iterator.fill(nb) {
          val nparents = indivsBySpecies(species).length
          val p1 = indivsBySpecies(species)(rng.nextInt(nparents))
          val p2 =
            // Have a chance to mate between different species
            if ((rng.nextDouble() < interSpeciesMatingProb) && (indivsBySpecies.size > 1)) {
              val otherSpecies = indivsBySpecies.keys.filter { _ != species }.toSeq(rng.nextInt(indivsBySpecies.size - 1))
              val nindivsOtherSpecies = indivsBySpecies(otherSpecies).length
              indivsBySpecies(otherSpecies)(rng.nextInt(nindivsOtherSpecies))
            } else indivsBySpecies(species)(rng.nextInt(nparents))
          //println(s"sampling species (${p1.genome.species}, ${p2.genome.species})")
          (p1, p2)
        }
    }.toIterator
  }

  /** Returns tuples (species, number of offsprings) */
  def speciesOffsprings(
    indivsBySpecies: Map[Int, Seq[Individual[G, P, F]]],
    totalOffsprings: Int): Seq[(Int, Int)]

  def crossover(
    i1: Individual[G, P, Double],
    i2: Individual[G, P, Double])(implicit rng: Random): G = {
    // - align the 2 parent genomes according to their innovation number
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

    result
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
  @tailrec final def alignGenomes(
    cg1: Seq[ConnectionGene],
    cg2: Seq[ConnectionGene],
    acc: List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)] = List.empty): List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)] = {
    if (cg1.isEmpty && cg2.isEmpty)
      acc
    else if (cg1.isEmpty)
      alignGenomes(Seq.empty, cg2.tail,
        (None, Some(cg2.head), Disjoint) :: acc)
    else if (cg2.isEmpty)
      alignGenomes(cg1.tail, Seq.empty,
        (Some(cg1.head), None, Disjoint) :: acc)
    else if (cg1.head.innovation == cg2.head.innovation)
      alignGenomes(cg1.tail, cg2.tail,
        (Some(cg1.head), Some(cg2.head), Aligned) :: acc)
    else if (cg1.head.innovation < cg2.head.innovation)
      alignGenomes(cg1.tail, cg2,
        (Some(cg1.head), None, Excess) :: acc)
    else
      alignGenomes(cg1, cg2.tail,
        (None, Some(cg2.head), Excess) :: acc)
  }

  def mutate(
    genome: G,
    state: BreedingState)(implicit rng: Random): (G, BreedingState) = {
    val r = rng.nextDouble()
    if (genome.connectionGenes.nonEmpty && r < mutationAddNodeProb) {
      mutateAddNode(genome, state)
    } else if (r < mutationAddNodeProb + mutationAddLinkProb) {
      mutateAddLink(genome, state)
    } else {
      // successively mutate weights and mutate enable bits on the genome
      val result = {
        mutateWeights(_: G)
      }.andThen {
        mutateEnabled(_: G)
      }(genome)
      (result, state)
    }
  }

  def pickNodesAddLink(genome: G)(implicit rng: Random): Option[(Int, Int)]

  def mutateAddLink(
    genome: G,
    state: BreedingState)(implicit rng: Random): (G, BreedingState) = {
    val pair = pickNodesAddLink(genome)

    pair match {
      case None => (genome, state)
      case Some((u, v)) =>
        val (gin, gnn, roi, ios) = state

        //Look for a LinkInnovation between nodes u and v in the record
        val sameInRoi: Option[LinkInnovation] = roi.collectFirst {
          case LinkInnovation(number, `u`, `v`) => LinkInnovation(number, u, v)
        }

        val thisInnov =
          sameInRoi match {
            case None => LinkInnovation(gin + 1, u, v)
            case Some(x) => x
          }

        val newroi =
          sameInRoi match {
            case None => roi :+ thisInnov
            case Some(x) => roi
          }

        val newgene =
          ConnectionGene(
            inNode = u,
            outNode = v,
            weight = initialWeight,
            enabled = true,
            innovation = thisInnov.innovnum)

        val newgenome =
          Genome(
            connectionGenes =
              genome.connectionGenes :+ newgene,
            nodes = genome.nodes,
            species = genome.species,
            lastNodeId = genome.lastNodeId)

        (newgenome, (gin + 1, gnn, newroi, ios))

    }
  }

  /** pick a connection gene, disable it and add 2 new connection genes */
  def mutateAddNode(
    genome: G,
    state: BreedingState)(implicit rng: Random): (G, BreedingState) = {
    val (gin, gnn, roi, ios) = state

    val picked: Int = rng.nextInt(genome.connectionGenes.length)
    val pickedcg = genome.connectionGenes(picked)

    val newNode = pickNewHiddenNode((genome.nodes(pickedcg.inNode).level + genome.nodes(pickedcg.outNode).level) / 2.0)

    //Look for a NodeInnovation between nodes u and v in the record
    val sameInRoi: Option[NodeInnovation] = roi.collectFirst {
      case NodeInnovation(a, b, c, `newNode`, pickedcg.inNode, pickedcg.outNode) => NodeInnovation(a, b, c, newNode, pickedcg.inNode, pickedcg.outNode)
    }

    val thisInnov =
      sameInRoi match {
        case None => NodeInnovation(gin + 1, gin + 2, gnn + 1, newNode, pickedcg.inNode, pickedcg.outNode)
        case Some(x) => x
      }

    val newcg1: ConnectionGene =
      ConnectionGene(
        inNode = pickedcg.inNode,
        outNode = thisInnov.newnodeId,
        weight = 1,
        enabled = true,
        innovation = thisInnov.innovnum1)

    val newcg2: ConnectionGene =
      ConnectionGene(
        inNode = thisInnov.newnodeId,
        outNode = pickedcg.outNode,
        weight = pickedcg.weight,
        enabled = true,
        innovation = thisInnov.innovnum2)

    val newconnectiongenes = genome.connectionGenes.updated(
      picked,
      pickedcg.copy(enabled = false)
    ) ++ Vector(newcg1, newcg2)

    val newnodes = genome.nodes + (thisInnov.newnodeId -> newNode)

    val newgenome = Genome(
      connectionGenes = newconnectiongenes,
      nodes = newnodes,
      species = genome.species,
      lastNodeId = thisInnov.newnodeId)

    val newroi =
      roi :+ thisInnov

    (newgenome, (gin + 2, gnn + 1, newroi, ios))
  }

  def pickNewHiddenNode(level: Double)(implicit rng: Random): HiddenNode

  def mutateWeights(
    genome: G)(implicit rng: Random): G =
    genome.copy(
      connectionGenes =
        genome.connectionGenes.map { (cg: ConnectionGene) =>
          cg.copy(weight = mutateWeight(cg.weight))
        })

  def mutateWeight(x: Double)(implicit rng: Random): Double = {
    val newweight =
      if (rng.nextDouble < mutationWeightProb0) 0
      else x + rng.nextGaussian() * mutationWeightSigma - (x * mutationWeightDriftTo0)
    max(min(mutationWeightHardMax, newweight), mutationWeightHardMin)
  }

  def mutateEnabled(
    g: G)(implicit rng: Random): G =
    g.copy(
      connectionGenes =
        g.connectionGenes.map { mutateEnabled })

  def mutateEnabled(cg: ConnectionGene)(implicit rng: Random): ConnectionGene = {
    val newenabled =
      if (cg.enabled) if (rng.nextDouble() < mutationDisableProb) !cg.enabled else cg.enabled
      else if (rng.nextDouble() < mutationEnableProb) !cg.enabled else cg.enabled
    cg.copy(enabled = newenabled)
  }

  /**
   * Sets the innovation numbers of each new mutation in the offsprings such that
   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
   * * mutations that are identical to one found it the archive's record of innovations are attributed its number
   */
  def setSpecies(
    genome: G,
    indexOfSpecies: IntMap[G],
    speciesCompatibilityThreshold: Double): (G, IntMap[G]) = {
    // Find a genome in the index of species which is sufficiently similar to the current genome.
    // The following code starts by checking if the offspring is compatible with the parent species (currently set in the species member of the offspring)
    // There is a problem when used with the adaptive species compatibility threshold: the number of species doesn't decrease since most of the time,
    // offsprings get attributed their parents species.
    if (useSpeciesHint && genomesDistance(genome, indexOfSpecies(genome.species)) < speciesCompatibilityThreshold)
      (genome, indexOfSpecies)
    else
      indexOfSpecies.find {
        case (sindex, prototype) => genomesDistance(prototype, genome) < speciesCompatibilityThreshold
      } match {
        //If found, attribute the species to the current genome
        case Some((species, _)) =>
          (genome.copy(species = species), indexOfSpecies)
        //Otherwise, create a new species with the current genome as the prototype
        case None =>
          val newspecies = indexOfSpecies.lastKey + 1
          val newgenome = genome.copy(species = newspecies)
          val newios = indexOfSpecies + ((newspecies, newgenome))
          (newgenome, newios)

      }
  }

  def genomesDistance(
    g1: G,
    g2: G): Double = {
    val alignment = alignGenomes(g1.connectionGenes, g2.connectionGenes)
    val (excess, disjoint, weightdiff, matchingGenes) = distanceFactors(alignment)
    val longestGenomeSize: Double = max(g1.connectionGenes.length, g2.connectionGenes.length).toDouble
    if (longestGenomeSize == 0) 0.0
    else {
      val structuralDistance = (genDistDisjointCoeff * excess / longestGenomeSize) +
        (genDistExcessCoeff * disjoint / longestGenomeSize)
      if (matchingGenes == 0)
        structuralDistance
      else
        structuralDistance + (genDistWeightDiffCoeff * weightdiff / matchingGenes.toDouble)
    }
  }

  @tailrec final def distanceFactors(
    alignment: List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)],
    acc: (Int, Int, Double, Int) = (0, 0, 0.0, 0)): (Int, Int, Double, Int) =
    alignment match {
      case List() => acc
      case head :: tail =>
        val (excess, disjoint, weightdiff, matchingGenes) = acc //distanceFactors(tail)
        val newacc = head match {
          case (Some(cg1), Some(cg2), Aligned) => (excess, disjoint, weightdiff + abs(cg1.weight - cg2.weight), matchingGenes + 1)
          case (_, _, Disjoint) => (excess, disjoint + 1, weightdiff, matchingGenes)
          case (_, _, Excess) => (excess + 1, disjoint, weightdiff, matchingGenes)
          case _ => throw new RuntimeException(s"Improper alignment: $head") //this case should never happen
        }
        distanceFactors(tail, newacc)

    }

}
*/ 
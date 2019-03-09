///*
// * Copyright (C) 17/09/2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//package mgo.evolution.mutation
//
//import mgo.Population
//import mgo.evolution.breed.NEATBreedingContext
//import mgo.evolution.genome.{ NEATGenomesAlign, NEATGenome }
//import mgo.evolution.genome.NEATGenome._
//import math.{ max, min, abs }
//import scala.annotation.tailrec
//import scala.collection.immutable.IntMap
//
//import scala.util.Random
//
//import scalaz._
//import Scalaz._
//
//import scala.language.higherKinds
//
//trait NEATMutation <: Mutation with NEATGenome with NEATBreedingContext with NEATGenomesAlign {
//
//  def mutationAddNodeProb: Double
//  def mutationAddLinkProb: Double
//  def mutationWeightProb0: Double
//  def mutationWeightSigma: Double
//  def mutationWeightDriftTo0: Double
//  def mutationWeightHardMax: Double
//  def mutationWeightHardMin: Double
//  def mutationDisableProb: Double
//  def mutationEnableProb: Double
//  def useSpeciesHint: Boolean
//  def genDistDisjointCoeff: Double
//  def genDistExcessCoeff: Double
//  def genDistWeightDiffCoeff: Double
//
//  def pickNodesAddLink(genome: G)(implicit rng: Random): Option[(Int, Int)]
//  def pickNewHiddenNode(level: Double)(implicit rng: Random): HiddenNode
//
//  def initialWeight(implicit rng: Random): Double = rng.nextGaussian() * mutationWeightSigma
//
//  override def mutate(genome: G, population: Population[G, P, F], archive: A)(implicit rng: Random): BreedingContext[G] = {
//    val r = rng.nextDouble()
//    val mutated: BreedingContext[G] = if (genome.connectionGenes.nonEmpty && r < mutationAddNodeProb) {
//      mutateAddNode(genome)
//    } else if (r < mutationAddNodeProb + mutationAddLinkProb) {
//      mutateAddLink(genome)
//    } else {
//      // successively mutate weights and mutate enable bits on the genome
//      val result = {
//        mutateWeights(_: G)
//      }.andThen {
//        mutateEnabled(_: G)
//      }(genome)
//
//      result.point[BreedingContext]
//    }
//
//    /*now that the offspring g was mutated, set species*/
//    val result = (mutated: BreedingContext[G]) >>= { g: G =>
//      State[BreedingState, G]({
//        case (gin, gnn, roi, ios) =>
//          val (newg, newios) = setSpecies(g, ios)
//          ((gin, gnn, roi, newios), newg)
//      })
//    }
//
//    result
//  }
//
//  def mutateAddLink(genome: G)(implicit rng: Random): BreedingContext[G] = {
//    val pair = pickNodesAddLink(genome)
//
//    pair match {
//      case None => genome.point[BreedingContext]
//      case Some((u, v)) => State[BreedingState, G]({
//        case (gin, gnn, roi, ios) =>
//
//          //Look for a LinkInnovation between nodes u and v in the record
//          val sameInRoi: Option[LinkInnovation] = roi.collectFirst {
//            case LinkInnovation(number, `u`, `v`) => LinkInnovation(number, u, v)
//          }
//
//          val thisInnov =
//            sameInRoi match {
//              case None => LinkInnovation(gin + 1, u, v)
//              case Some(x) => x
//            }
//
//          val newroi =
//            sameInRoi match {
//              case None => roi :+ thisInnov
//              case Some(x) => roi
//            }
//
//          val newgene =
//            ConnectionGene(
//              inNode = u,
//              outNode = v,
//              weight = initialWeight,
//              enabled = true,
//              innovation = thisInnov.innovnum)
//
//          val newgenome =
//            Genome(
//              connectionGenes =
//                genome.connectionGenes :+ newgene,
//              nodes = genome.nodes,
//              species = genome.species,
//              lastNodeId = genome.lastNodeId)
//
//          ((gin + 1, gnn, newroi, ios), newgenome)
//      })
//    }
//  }
//
//  /** pick a connection gene, disable it and add 2 new connection genes */
//  def mutateAddNode(genome: G)(implicit rng: Random): BreedingContext[G] = State[BreedingState, G]({
//    case (gin, gnn, roi, ios) =>
//
//      val picked: Int = rng.nextInt(genome.connectionGenes.length)
//      val pickedcg = genome.connectionGenes(picked)
//
//      val newNode = pickNewHiddenNode((genome.nodes(pickedcg.inNode).level + genome.nodes(pickedcg.outNode).level) / 2.0)
//
//      //Look for a NodeInnovation between nodes u and v in the record
//      val sameInRoi: Option[NodeInnovation] = roi.collectFirst {
//        case NodeInnovation(a, b, c, `newNode`, pickedcg.inNode, pickedcg.outNode) => NodeInnovation(a, b, c, newNode, pickedcg.inNode, pickedcg.outNode)
//      }
//
//      val thisInnov =
//        sameInRoi match {
//          case None => NodeInnovation(gin + 1, gin + 2, gnn + 1, newNode, pickedcg.inNode, pickedcg.outNode)
//          case Some(x) => x
//        }
//
//      val newcg1: ConnectionGene =
//        ConnectionGene(
//          inNode = pickedcg.inNode,
//          outNode = thisInnov.newnodeId,
//          weight = 1,
//          enabled = true,
//          innovation = thisInnov.innovnum1)
//
//      val newcg2: ConnectionGene =
//        ConnectionGene(
//          inNode = thisInnov.newnodeId,
//          outNode = pickedcg.outNode,
//          weight = pickedcg.weight,
//          enabled = true,
//          innovation = thisInnov.innovnum2)
//
//      val newconnectiongenes = genome.connectionGenes.updated(
//        picked,
//        pickedcg.copy(enabled = false)
//      ) ++ Vector(newcg1, newcg2)
//
//      val newnodes = genome.nodes + (thisInnov.newnodeId -> newNode)
//
//      val newgenome = Genome(
//        connectionGenes = newconnectiongenes,
//        nodes = newnodes,
//        species = genome.species,
//        lastNodeId = thisInnov.newnodeId)
//
//      val newroi =
//        roi :+ thisInnov
//
//      ((gin + 2, gnn + 1, newroi, ios), newgenome)
//  })
//
//  def mutateWeights(genome: G)(implicit rng: Random): G =
//    genome.copy(
//      connectionGenes =
//        genome.connectionGenes.map { (cg: ConnectionGene) =>
//          cg.copy(weight = mutateWeight(cg.weight))
//        })
//
//  def mutateWeight(x: Double)(implicit rng: Random): Double = {
//    val newweight =
//      if (rng.nextDouble < mutationWeightProb0) 0
//      else x + rng.nextGaussian() * mutationWeightSigma - (x * mutationWeightDriftTo0)
//    max(min(mutationWeightHardMax, newweight), mutationWeightHardMin)
//  }
//
//  def mutateEnabled(g: G)(implicit rng: Random): G =
//    g.copy(
//      connectionGenes =
//        g.connectionGenes.map { mutateEnabled })
//
//  def mutateEnabled(cg: ConnectionGene)(implicit rng: Random): ConnectionGene = {
//    val newenabled =
//      if (cg.enabled) if (rng.nextDouble() < mutationDisableProb) !cg.enabled else cg.enabled
//      else if (rng.nextDouble() < mutationEnableProb) !cg.enabled else cg.enabled
//    cg.copy(enabled = newenabled)
//  }
//
//  /**
//   * Sets the innovation numbers of each new mutation in the offsprings such that
//   * * unique mutations are attributed a unique number greater than the archive's global innovation number,
//   * * mutations that are identical to one found it the archive's record of innovations are attributed its number
//   */
//  def setSpecies(
//    genome: G,
//    indexOfSpecies: IntMap[G]): (G, IntMap[G]) = {
//    // Find a genome in the index of species which is sufficiently similar to the current genome.
//    // The following code starts by checking if the offspring is compatible with the parent species (currently set in the species member of the offspring)
//    // There is a problem when used with the adaptive species compatibility threshold: the number of species doesn't decrease since most of the time,
//    // offsprings get attributed their parents species.
//    if (useSpeciesHint && genomesDistance(genome, indexOfSpecies(genome.species)) < speciesCompatibilityThreshold)
//      (genome, indexOfSpecies)
//    else
//      indexOfSpecies.find {
//        case (sindex, prototype) => genomesDistance(prototype, genome) < speciesCompatibilityThreshold
//      } match {
//        //If found, attribute the species to the current genome
//        case Some((species, _)) =>
//          (genome.copy(species = species), indexOfSpecies)
//        //Otherwise, create a new species with the current genome as the prototype
//        case None =>
//          val newspecies = indexOfSpecies.lastKey + 1
//          val newgenome = genome.copy(species = newspecies)
//          val newios = indexOfSpecies + ((newspecies, newgenome))
//          (newgenome, newios)
//
//      }
//  }
//
//  def genomesDistance(g1: G, g2: G): Double = {
//    val alignment = alignGenomes(g1.connectionGenes, g2.connectionGenes)
//    val (excess, disjoint, weightdiff, matchingGenes) = distanceFactors(alignment)
//    val longestGenomeSize: Double = max(g1.connectionGenes.length, g2.connectionGenes.length).toDouble
//    if (longestGenomeSize == 0) 0.0
//    else {
//      val structuralDistance = (genDistDisjointCoeff * excess / longestGenomeSize) +
//        (genDistExcessCoeff * disjoint / longestGenomeSize)
//      if (matchingGenes == 0)
//        structuralDistance
//      else
//        structuralDistance + (genDistWeightDiffCoeff * weightdiff / matchingGenes.toDouble)
//    }
//  }
//
//  import AlignmentInfo._
//  @tailrec final def distanceFactors(
//    alignment: List[(Option[ConnectionGene], Option[ConnectionGene], AlignmentInfo)],
//    acc: (Int, Int, Double, Int) = (0, 0, 0.0, 0)): (Int, Int, Double, Int) =
//    alignment match {
//      case List() => acc
//      case head :: tail =>
//        val (excess, disjoint, weightdiff, matchingGenes) = acc //distanceFactors(tail)
//        val newacc = head match {
//          case (Some(cg1), Some(cg2), Aligned) => (excess, disjoint, weightdiff + abs(cg1.weight - cg2.weight), matchingGenes + 1)
//          case (_, _, Disjoint) => (excess, disjoint + 1, weightdiff, matchingGenes)
//          case (_, _, Excess) => (excess + 1, disjoint, weightdiff, matchingGenes)
//          case _ => throw new RuntimeException(s"Improper alignment: $head") //this case should never happen
//        }
//        distanceFactors(tail, newacc)
//
//    }
//
//}

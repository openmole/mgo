///*
// * Copyright (C) 2015 Guillaume Chérel
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
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
//
//package fr.iscpif.mgo.genome
//
//import fr.iscpif.mgo.breed.NEATBreedingContext
//
//import collection.immutable.IntMap
//import util.Random
//import math._
//
//import scalaz._
//import Scalaz._
//
//import NEATGenome._
//
//trait NEATMinimalGenomeConnectedIO <: NEATInitialGenome {
//
//  override def minimalGenome(implicit rng: Random): G =
//    Genome(
//      connectionGenes =
//        inputNodesIndices.flatMap { u =>
//          outputNodesIndices.map {
//            (u, _)
//          }
//        }
//          .zipWithIndex.map {
//            case ((u, v), i) =>
//              ConnectionGene(
//                inNode = u,
//                outNode = v,
//                weight = max(min(mutationWeightHardMax, rng.nextGaussian() * mutationWeightSigma), mutationWeightHardMin),
//                enabled = true,
//                innovation = i)
//          },
//      nodes =
//        IntMap(
//          inputNodesIndices.map {
//            _ -> newInputNode
//          }
//            ++ biasNodesIndices.map {
//              _ -> newBiasNode
//            }
//            ++ outputNodesIndices.map {
//              _ -> newOutputNode
//            }.toSeq: _*),
//      species = 0,
//      lastNodeId = inputNodes + biasNodes + outputNodes - 1
//    )
//
//  def newInputNode: InputNode
//  def newBiasNode: BiasNode
//  def newOutputNode: OutputNode
//
//  def mutationWeightHardMin: Double
//  def mutationWeightHardMax: Double
//  def mutationWeightSigma: Double
//
//}

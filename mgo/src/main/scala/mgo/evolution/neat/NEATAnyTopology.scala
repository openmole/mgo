///*
// * Copyright (C) 2015 Guillaume Chérel
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
//
//package mgo.evolution.breed
//
//import mgo.evolution._
//import util.Random
//import mgo.evolution.genome.RandomGenome
//import mgo.evolution.genome.NEATGenome
//import mgo.evolution.archive.NEATArchive
//import collection.immutable.IntMap
//
//trait NEATAnyTopology extends NEATGenome {
//
//  def mutationAddLinkBiasProb: Double
//
//  def pickNodesAddLink(genome: G)(implicit rng: Random): Option[(Int, Int)] = {
//    val connections =
//      IntMap[Seq[Int]](
//        genome.connectionGenes.map { cg => cg.inNode -> cg.outNode }
//          .groupBy { (_: (Int, Int))._1 }
//          .mapValues { (_: Seq[(Int, Int)]).map { _._2 } }.toSeq: _*)
//
//    val pair: Option[(Int, Int)] =
//      (if (rng.nextDouble() < mutationAddLinkBiasProb)
//        rng.shuffle(biasNodesIndices.iterator)
//      else
//        rng.shuffle(genome.nodes.keysIterator))
//        .flatMap { u => rng.shuffle(genome.nodes.keysIterator).map { v => (u, v) } }
//        .find {
//          case (u: Int, v: Int) =>
//            !connections(u).contains(v)
//        }
//
//    pair
//  }
//}
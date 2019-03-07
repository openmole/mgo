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
//package mgo.evolution.genome
//
//import mgo.evolution.breed.NEATBreedingContext
//import mgo.evolution.genome.NEATGenome.{ ConnectionGene, Genome }
//
//import scala.collection.immutable.IntMap
//import scala.math._
//import scala.util.Random
//import scalaz.State
//
//trait NEATInitialGenome <: InitialGenome with NEATGenome with NEATBreedingContext {
//
//  /**
//   *
//   * The introduction of an initial genome should harmonize the breeding state and the new genome by:
//   * - setting the global innovation number to the highest genome's innovation number, if it's higher than the current global innovation number,
//   * - same with the global node number,
//   * - setting the genome's species to one found in the index of species if any, and add the genome to the index otherwise.
//   */
//  def initialGenome(implicit rng: Random): BreedingContext[G] = {
//    val g: G = minimalGenome(rng)
//
//    State(s => {
//      val (gin, gnn, roi, ios) = s
//
//      val newgin =
//        if (g.connectionGenes.isEmpty) gin
//        else
//          max(
//            gin,
//            g.connectionGenes.map { _.innovation }.max
//          )
//
//      val newgnn = max(
//        gnn,
//        g.lastNodeId
//      )
//
//      // This initial minimal genome is always species 0. So just add it to the intmap if not there
//      val newios = if (ios.contains(0)) ios else ios + ((0, g))
//
//      ((newgin, newgnn, roi, newios), g)
//
//    }
//    )
//  }
//
//  def minimalGenome(implicit rng: Random): G
//}

///*
// * Copyright (C) 16/09/2015 Guillaume Chérel
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
//package mgo.evolution.breed
//
//import mgo.evolution.archive.NEATArchive
//import mgo.evolution.genome.NEATGenome.LinkInnovation
//import mgo.{ Population, Individual }
//import mgo.evolution.genome.{ NEATGenome }
//
//import scala.collection.immutable.IntMap
//import scala.math._
//import scala.util.Random
//import scalaz._
//import Scalaz._
//
//trait NEATBreedingContext extends BreedingContext with NEATGenome with NEATArchive {
//
//  type BreedingState = (Int, //global innovation number
//  Int, //global node number
//  Seq[Innovation], //record of innovations
//  IntMap[G]) //index of species
//
//  type BreedingContext[Z] = State[BreedingState, Z]
//
//  override implicit def monadBreedingContext: cats.Monad[BreedingContext] = new cats.Monad[BreedingContext] {
//    def bind[A, B](fa: BreedingContext[A])(f: (A) ⇒ BreedingContext[B]): BreedingContext[B] = State({ s1 =>
//      val (s2, a) = fa.run(s1)
//      val (s3, b) = f(a).run(s2)
//      (s3, b)
//    })
//
//    def point[A](a: ⇒ A): BreedingContext[A] = State(s => (s, a))
//  }
//
//  /** extract the content from the breeding monad */
//  override def unwrapBreedingContext[Z](x: BreedingContext[Z], population: Population[G, P, F], archive: A): Z = {
//
//    // First, compute the initial state from the population and archive
//
//    val (globalInnovationNumber, globalNodeNumber) = population.foldLeft((0, 0)) {
//      case (acc, indiv) =>
//        val (gin, gnn) = acc
//        val newgin = if (indiv.genome.connectionGenes.isEmpty) 0 else max(gin, indiv.genome.connectionGenes.map {
//          _.innovation
//        }.max)
//        val newgnn = max(gnn, indiv.genome.lastNodeId)
//        (newgin, newgnn)
//    }
//
//    val initialState = (globalInnovationNumber, globalNodeNumber, Seq.empty, archive.indexOfSpecies)
//
//    x.run(initialState)._2
//  }
//
//}

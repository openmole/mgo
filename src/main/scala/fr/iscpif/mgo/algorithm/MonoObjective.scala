///*
// * Copyright (C) 2014 Romain Reuillon
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
//package fr.iscpif.mgo.algorithm
//
//import fr.iscpif.mgo._
//import scalaz._
//
//trait MonoObjective <: Algorithm with GeneticAlgorithm with AllFunctions {
//
//  type STATE = Unit
//  def initialState = Unit
//
//  implicit def fitness: Fitness[Double]
//  implicit def mergeClones = youngest
//
//  def mu: Int
//
//  override def breeding(pop: Pop, lambda: Int): State[AlgorithmState, Vector[G]] =
//    onRank.apply(pop) flatMap { challenged =>
//      def fight = tournament(challenged, pop)
//      newGenomes(fight, pop).map(_.map(clamp).toVector).generateFlat(lambda)
//    }
//
//  override def elitism(population: Pop, offspring: Pop): State[AlgorithmState, Pop] =
//    for {
//      p1 <- merge(population, offspring)
//      p2 <- applyCloneStrategy(p1)
//      p3 <- removeNaN(p2)
//      p4 <- keepBest(mu, p3)
//    } yield p4
//
//}
///*
// * Copyright (C) 2012 Romain Reuillon
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
//package fr.iscpif.mgo.genome
//
//import monocle._
//import monocle.macros._
//import scala.util.Random
//
//object GAGenomeWithSigma {
//
//}
//
//case class GAGenomeWithSigma[G](
//  values: Array[Double],
//  sigma: Array[Double],
//  mutation: Option[Int] = None,
//  crossover: Option[Int] = None)
//
///**
// * Genome for genetic algorithm with an autoadaptative sigma component
// */
//trait GAGenomeWithSigma extends GA with Sigma with DynamicApplication {
//
//  type G = GAGenomeWithSigma.Genome
//
//  def rawValues = Lens((_: G).values.toSeq)(v => g => g.copy(values = v.toArray))
//  def sigma = Lens((_: G).sigma.toSeq)(v => g => g.copy(sigma = v.toArray))
//  def fromMutation = Lenser[G](_.mutation)
//  def fromCrossover = Lenser[G](_.crossover)
//
//  def randomGenome(implicit rng: Random) = {
//    def rnd = Stream.continually(rng.nextDouble).take(genomeSize).toArray
//    GAGenomeWithSigma.Genome(rnd, rnd)
//  }
//
//}
//

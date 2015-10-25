/*
 * Copyright (C) 2012 Romain Reuillon
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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import monocle.syntax._
import tools._
import org.apache.commons.math3.distribution.CauchyDistribution
import util.Random
import scala.math._

import scalaz._
import Scalaz._

import monocle.Lens
import monocle.syntax._

import scala.language.higherKinds
//
///**
// * Mutation of a genome based on gausian distribution arrount the genome with
// * adaptive sigma values.
// * See on the web : http://www.nashcoding.com/2010/07/07/evolutionary-algorithms-the-little-things-youd-never-guess-part-1/#fnref-28-1
// * See on paper : Gaussian mutation and self adaptation (Hinterding) &&
// * Parameter Control in Evolutionary Algorithms (Agoston Endre Eiben, Robert
// * Hinterding, and Zbigniew Michalewicz, Senior Member, IEEE) + How to Solve It,
// * Modern Heuristics
// */
//object AdaptiveCauchyMutation {
//
//  //  def apply(mutation: Mutation with Sigma with GA with MinimumSigma): mutation.Mutation = {
//  //    import mutation._
//  //    (genome: G, population: Population[G, P, F], archive: A, rng: Random) => {
//  //      val newSigma = sigma.get(genome).map { s => math.max(minimumSigma, s * exp(rng.nextGaussian)) }
//  //
//  //      val newValues =
//  //        (values.get(genome) zip newSigma) map {
//  //          case (v, s) => new CauchyDistribution(rng, v, s).sample //.nextGaussian * s + v
//  //        }
//  //
//  //      newValues.foreach(v => assert(!v.isNaN))
//  //      (genome &|-> values set newValues) &|-> sigma set newSigma
//  //    }
//  //  }
//
//  def apply[G, P, F, A, BreedingContext[_]: Monad](minimumSigma: Double)(values: Lens[G, Seq[Double]], sigma: Lens[G, Seq[Double]]): (G, Population[G, P, F], A, Random) => BreedingContext[G] =
//    (genome: G, population: Population[G, P, F], archive: A, rng: Random) => {
//      val newSigma = sigma.get(genome).map { s => math.max(minimumSigma, s * exp(rng.nextGaussian)) }
//
//      val newValues =
//        (values.get(genome) zip newSigma) map {
//          case (v, s) => new CauchyDistribution(rng, v, s).sample //.nextGaussian * s + v
//        }
//
//      newValues.foreach(v => assert(!v.isNaN))
//      ((genome &|-> values set newValues) &|-> sigma set newSigma).point[BreedingContext]
//    }
//
//}

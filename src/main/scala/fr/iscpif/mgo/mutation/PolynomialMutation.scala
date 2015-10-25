///*
// * Copyright (C) 2011 Sebastien Rey
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
// * You should have received a copy of the GNU Affero General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.iscpif.mgo.mutation
//
//import fr.iscpif.mgo._
//import util.Random
//import scala.math._
//
//import scalaz._
//import Scalaz._
//
//import monocle.Lens
//import monocle.syntax._
//
//import scala.language.higherKinds
//
///**
// * Polynomial mutationolynomial mutation by Deb and Goyal. If is the value of
// * the ith parameter selected for mutation with a probability pm and the result
// * of the mutation is the new value obtained by a polynomial probability
// * distribution.
// * Based on the source code of Jmetal library
// * Author : Antonio J. Nebro <antonio@lcc.uma.es> and Juan J. Durillo <durillo@lcc.uma.es>
// */
//object PolynomialMutation {
//
//  //  def apply(mutation: Mutation with GA)(distributionIndex: Double, mutationRate: Double = 0.5): mutation.Mutation = {
//  //    import mutation._
//  //    (g: G, population: Population[G, P, F], archive: A, rng: Random) => {
//  //      val newValues = values.get(g) map {
//  //        v =>
//  //          if (rng.nextDouble <= mutationRate) {
//  //            val yl = 0.0 // lower bound
//  //            val yu = 1.0 // upper bound
//  //            val delta1 = (v - yl) / (yu - yl)
//  //            val delta2 = (yu - v) / (yu - yl)
//  //            val mut_pow = 1.0 / (distributionIndex + 1.0)
//  //            val rnd = rng.nextDouble
//  //
//  //            val deltaq: Double = (if (rnd <= 0.5) {
//  //              val xy = 1.0 - delta1
//  //              val value = 2.0 * rnd + (1.0 - 2.0 * rnd) * (pow(xy, (distributionIndex + 1.0)))
//  //              pow(value, mut_pow) - 1.0
//  //            } else {
//  //              val xy = 1.0 - delta2
//  //              val value = 2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * (pow(xy, (distributionIndex + 1.0)))
//  //              1.0 - (pow(value, mut_pow))
//  //            })
//  //
//  //            val finalValue = v + deltaq * (yu - yl)
//  //
//  //            if (finalValue < yl) yl
//  //            else if (finalValue > yu) yu
//  //            else finalValue
//  //          }
//  //          v
//  //      }
//  //      values.set(newValues)(g)
//  //    }
//  //  }
//
//  def apply[G, P, F, A, BreedingContext[_]: Monad](distributionIndex: Double, mutationRate: Double = 0.5)(values: Lens[G, Seq[Double]]): (G, Population[G, P, F], A, Random) => BreedingContext[G] = {
//    (g: G, population: Population[G, P, F], archive: A, rng: Random) =>
//      {
//        val newValues = values.get(g) map {
//          v =>
//            if (rng.nextDouble <= mutationRate) {
//              val yl = 0.0 // lower bound
//              val yu = 1.0 // upper bound
//              val delta1 = (v - yl) / (yu - yl)
//              val delta2 = (yu - v) / (yu - yl)
//              val mut_pow = 1.0 / (distributionIndex + 1.0)
//              val rnd = rng.nextDouble
//
//              val deltaq: Double = (if (rnd <= 0.5) {
//                val xy = 1.0 - delta1
//                val value = 2.0 * rnd + (1.0 - 2.0 * rnd) * (pow(xy, (distributionIndex + 1.0)))
//                pow(value, mut_pow) - 1.0
//              } else {
//                val xy = 1.0 - delta2
//                val value = 2.0 * (1.0 - rnd) + 2.0 * (rnd - 0.5) * (pow(xy, (distributionIndex + 1.0)))
//                1.0 - (pow(value, mut_pow))
//              })
//
//              val finalValue = v + deltaq * (yu - yl)
//
//              if (finalValue < yl) yl
//              else if (finalValue > yu) yu
//              else finalValue
//            }
//            v
//        }
//        values.set(newValues)(g).point[BreedingContext]
//      }
//  }
//}
//

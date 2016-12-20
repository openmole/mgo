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
//package mgo.breed
//
//import mgo._
//import org.apache.commons.math3.linear.Array2DRowRealMatrix
//
//import scalaz._
//import Scalaz._
//
//import monocle.syntax._
//
//import scala.util.Random
//
//trait CMAESBreeding <: Breeding with GA with CMAESArchive {
//  def breed(population: Population[G, P, F], a: A)(implicit rng: Random): Seq[G] = {
//    // Generate lambda offspring
//    val arz = randn1(genomeSize, lambda)
//
//    (0 until lambda).map {
//      k =>
//        val v = a.xmean.add(a.BD.multiply(arz.getColumnMatrix(k)).scalarMultiply(a.sigma))
//        assert(!v.getColumn(0).exists(_.isNaN), a.C.getData.map(_.mkString(",")).mkString("\n"))
//        (randomGenome &|-> values set v.getColumn(0)) &|-> randomValues set arz.getColumn(k)
//    }
//
//    //    final RealMatrix arx = zeros(dimension, lambda);
//    //    final double[] fitness = new double[lambda];
//    //    final ValuePenaltyPair[] valuePenaltyPairs = new ValuePenaltyPair[lambda];
//    //    // generate random offspring
//    //    for (int k = 0; k < lambda; k++) {
//    //      RealMatrix arxk = null;
//    //      for (int i = 0; i < checkFeasableCount + 1; i++) {
//    //        if (diagonalOnly <= 0) {
//    //          arxk = xmean.add(BD.multiply(arz.getColumnMatrix(k))
//    //            .scalarMultiply(sigma)); // m + sig * Normal(0,C)
//    //        } else {
//    //          arxk = xmean.add(times(diagD,arz.getColumnMatrix(k))
//    //            .scalarMultiply(sigma));
//    //        }
//    //        if (i >= checkFeasableCount ||
//    //          fitfun.isFeasible(arxk.getColumn(0))) {
//    //          break;
//    //        }
//    //        // regenerate random arguments for row
//    //        arz.setColumn(k, randn(dimension));
//    //      }
//    //      copyColumn(arxk, 0, arx, k);
//    //      try {
//    //        valuePenaltyPairs[k] = fitfun.value(arx.getColumn(k)); // compute fitness
//    //      } catch (TooManyEvaluationsException e) {
//    //        break generationLoop;
//    //      }
//    //    }
//    //
//    //    // Compute fitnesses by adding value and penalty after scaling by value range.
//    //    double valueRange = valueRange(valuePenaltyPairs);
//    //    for (int iValue=0;iValue<valuePenaltyPairs.length;iValue++) {
//    //      fitness[iValue] = valuePenaltyPairs[iValue].value + valuePenaltyPairs[iValue].penalty*valueRange;
//    //    }
//  }
//
//  /**
//   * @param size Number of rows.
//   * @param popSize Population size.
//   * @return a 2-dimensional matrix of Gaussian random numbers.
//   */
//  private def randn1(size: Int, popSize: Int)(implicit rng: Random) = {
//    val d = Array.tabulate[Double](size, popSize) {
//      (r, c) => rng.nextGaussian
//    }
//    new Array2DRowRealMatrix(d, false)
//  }
//}

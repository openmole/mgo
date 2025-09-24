/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
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

package mgo.evolution.algorithm

//import mgo.evolution._

/*trait CMAES <: Evolution
  with KeepOffspringElitism
  with GAGenomeWithRandomValue
  with MaxAggregation
  with CMAESBreeding
  with CMAESArchive
  with ClampedGenome*/



object CMAESOperation:

  class CMAEvolutionStrategy (
    iteration: Int,
    mu: Int,
    lambda: Int,
    n: Int, // dimension
//    ps: IArray[Double],
//    pc: IArray[Double],
//    b: IArray[IArray[Double]],
//    c: IArray[IArray[Double]],
//    d: IArray[Double],
//    sigma: Double,
//    xMean: IArray[Double]
                                           ):

    //private val mu = math.floor(lambda / 2).toInt

    val (weights, mueff): (IArray[Double], Double) =
      val w = IArray.tabulate(mu): i =>
        Math.log(mu + 1.0 / 2.0) - math.log(1.0 + i)
      val sumW = w.sum
      val weights = w.map(_ / sumW)
      (weights, weights.sum / weights.map(w => w * w).sum)

    val cs = (mueff + 2) / (n + mueff + 3)
//
//    private val cc = 4.0 / (n + 4.0)
//
//    private val c1 = 2 / (math.pow(n + 1.3, 2) + mueff)
//
//    private val cmu = min(1 - c1, 2 * (mueff - 2 + 1 / mueff) / (math.pow(n + 2, 2) + mueff))
//
//    private val chiN = math.sqrt(n) * (1.0 - 1.0 / (4.0 * n) + 1.0 / (21.0 * n * n))
//
//    private val damps = 1.0 + 2.0 * math.max(0.0, math.sqrt((mueff - 1.0) / (n + 1.0)) - 1.0) + cs

//    /**
//     * Generate a new population of solutions.
//     *
//     * @return a new generation of solutions.
//     */
//    def samplePopulation(): DenseMatrix[Double] = {
//
//      val g = breeze.stats.distributions.Gaussian(0, 1)
//
//      val s = (0 until lambda) map {
//        _ =>
//          xMean + sigma * b * (d :* g.samplesVector(n))
//      }
//
//      val distribution = DenseMatrix(new DenseVector(s.toArray).valuesIterator.map(_.valuesIterator.toArray).toSeq: _*)
//
//      distribution
//
//    }

//    /**
//     * Update search distribution.
//     *
//     * @param population current population.
//     * @param fitness    fitness of current population.
//     * @return a copy of CMAEvolutionStrategy with updated state.
//     */
//    def updateDistribution(population: DenseMatrix[Double], fitness: DenseVector[Double]): CMAEvolutionStrategy = {
//
//      val arfitness = argsort(fitness)
//
//      val selected = DenseVector((0 until mu).map {
//        idx => population(arfitness(idx), ::).inner
//      } toArray)
//
//      val newXMean = DenseVector.zeros[Double](n).mapPairs {
//        case (idx, _) =>
//          sum(selected.map(_(idx)) :* weights)
//      }
//
//      val invsqrtC = b * diag(d.:^(-1.0)) * b.t
//
//      val psN: DenseVector[Double] = (1.0 - cs) * ps + sqrt(cs * (2.0 - cs) * mueff) * invsqrtC * (newXMean - xMean) / sigma
//
//      val hsig = if (norm(psN) / math.sqrt(1.0 - pow(1.0 - cs, 2.0 * (iteration + 1))) / chiN < 1.4 + 2.0 / (n + 1.0)) 1.0 else 0.0
//
//      val pcN: DenseVector[Double] = (1.0 - cc) * pc + hsig * sqrt(cc * (2.0 - cc) * mueff) * (newXMean - xMean) / sigma
//
//      val artmp: DenseVector[DenseVector[Double]] = selected.map {
//        s => (s - xMean) :/ sigma
//      }
//
//      val artmpm = DenseMatrix(artmp.valuesIterator.map(_.valuesIterator.toArray).toSeq: _*).t
//
//      val base = (1.0 - c1 - cmu) * c
//
//      val plusRankOne = c1 * (pcN * pcN.t + (1.0 - hsig) * cc * (2.0 - cc) * c)
//
//
//      val rankMu = cmu * artmpm * diag(weights) * artmpm.t
//      val nC = base + plusRankOne + rankMu
//
//      val sigmaN = sigma * math.exp((cs / damps) * ((norm(psN) / chiN) - 1.0))
//
//      val psxps = sum(psN :* psN)
//
//      val sigmaNN = sigma * math.exp(((math.sqrt(psxps) / chiN) - 1.0) * cs / damps)
//
//      val EigSym(nD, nB) = eigSym(nC)
//
//      new CMAEvolutionStrategy(
//        iteration + 1,
//        lambda,
//        n,
//        psN,
//        pcN,
//        nB,
//        nC,
//        sqrt(nD),
//        sigmaN,
//        newXMean
//      )
//
//    }



//
//  object CMAEvolutionStrategy {
//    /**
//     * Instanciates a copy of CMAEvolutionStrategy from initial population with given initial distribution.
//     *
//     * @param lambda     population size.
//     * @param initialX   initial solution.
//     * @param initialStd initial standard deviation of first population.
//     * @return an instance of [[cmaes.CMAEvolutionStrategy]]
//     */
//    def apply(lambda: Int, initialX: DenseVector[Double], initialStd: DenseVector[Double]): CMAEvolutionStrategy = {
//
//      new CMAEvolutionStrategy(
//        1,
//        lambda,
//        initialX.length,
//        DenseVector.zeros[Double](initialX.length),
//        DenseVector.zeros[Double](initialX.length),
//        DenseMatrix.eye[Double](initialX.length),
//        diag(initialStd),
//        initialStd,
//        1.0,
//        initialX)
//    }
//
//  }
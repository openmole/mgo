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

package fr.iscpif.mgo.archive

import fr.iscpif.mgo._
import fr.iscpif.mgo.mutation.MinimumSigma
import org.apache.commons.math3.linear.{ EigenDecomposition, MatrixUtils, Array2DRowRealMatrix, RealMatrix }
import org.apache.commons.math3.util.{ MathUtils, FastMath, MathArrays }
import monocle.syntax._

import scala.util.Random

object CMAESArchive {

  case class FIFO[T](val content: List[T]) {
    def +=(t: T, size: Int): FIFO[T] = copy(content = t :: content.slice(0, size - 1))
  }

  case class Archive(
    xmean: RealMatrix,
    B: RealMatrix,
    D: RealMatrix,
    C: RealMatrix,
    BD: RealMatrix,
    diagC: RealMatrix,
    diagD: RealMatrix,
    sigma: Double,
    ps: RealMatrix,
    pc: RealMatrix,
    normps: Double,
    fitnessHistory: FIFO[Double],
    bestValue: Double,
    iterations: Long)

}

trait CMAESArchive <: Archive with Aggregation with GA with RandomValue with MinimumSigma { ar =>

  def initialSigma = 0.3
  def guess(implicit rng: Random) = Seq.fill[Double](genomeSize)(0.5)
  def negminresidualvariance = 0.66
  // where to make up for the variance loss
  def negalphaold = 0.5
  def mu(lambda: Int) = math.ceil(lambda / 2.0).toInt
  def activeCMAES = true

  type A = CMAESArchive.Archive
  def initialArchive(implicit rng: Random): A = {
    // initialize sigma

    // final double[][] sigmaArray = new double[guess.length][1];
    //for (int i = 0; i < guess.length; i++) {
    //  sigmaArray[i][0] = inputSigma[i];
    //}
    val sigmaArray = Array.fill[Double](genomeSize, 1)(initialSigma)
    val insigma = new Array2DRowRealMatrix(sigmaArray, false)
    val sigma = initialSigma // max(insigma); // overall standard deviation

    // initialize termination criteria
    /*stopTolUpX = 1e3 * max(insigma);
    stopTolX = 1e-11 * max(insigma);
    stopTolFun = 1e-12;
    stopTolHistFun = 1e-13;*/

    // initialize selection strategy parameters
    //mu = lambda / 2; // number of parents/points for recombination

    /*double sumw = 0;
    double sumwq = 0;
    for (int i = 0; i < mu; i++) {
      double w = weights.getEntry(i, 0);
      sumw += w;
      sumwq += w * w;
    }*/

    // variance-effectiveness of sum w_i x_i

    // initialize dynamic strategy parameters and constants
    //val cc = (4 + mueff / dimension) / (dimension + 4 + 2 * mueff / dimension)

    //damps = 1 + 2*max(0, sqrt((mueff-1)/(N+1))-1) + cs;
    //(1 + 2 * FastMath.max(0, FastMath.sqrt((mueff - 1) / (genomeSize + 1)) - 1)) *
    //  FastMath.max(0.3, 1 - genomeSize / (1e-6 + maxIterations)) + cs; // minor increment

    //val ccov1Sep = FastMath.min(1, ccov1 * (genomeSize + 1.5) / 3)
    //val ccovmuSep = FastMath.min(1 - ccov1, ccovmu * (genomeSize + 1.5) / 3)

    // intialize CMA internal values - updated each generation
    val xmean = MatrixUtils.createColumnRealMatrix(guess.toArray) // objective variables
    val diagD = insigma.scalarMultiply(1 / sigma)
    val diagC = square(diagD)
    val pc = zeros(genomeSize, 1) // evolution paths for C and sigma
    val ps = zeros(genomeSize, 1) // B defines the coordinate system
    val normps = ps.getFrobeniusNorm

    val B = eye(genomeSize, genomeSize)
    val D = ones(genomeSize, 1) // diagonal D defines the scaling
    val BD = times(B, repmat(diagD.transpose(), genomeSize, 1))
    val C = B.multiply(diag(square(D)).multiply(B.transpose())) // covariance
    CMAESArchive.Archive(
      xmean = xmean,
      B = B,
      D = D,
      C = C,
      BD = BD,
      diagC = diagC,
      diagD = diagD,
      sigma = sigma,
      ps = ps,
      pc = pc,
      normps = normps,
      fitnessHistory = CMAESArchive.FIFO(List(Double.PositiveInfinity)),
      bestValue = Double.PositiveInfinity,
      iterations = 0)
  }

  override def archive(a: A, oldIndividuals: Population[G, P, F], offspring: Population[G, P, F])(implicit rng: Random): A = {
    lazy val population = offspring

    lazy val lambda = population.size
    lazy val mu = ar.mu(lambda)

    lazy val logMu2 = FastMath.log(mu + 0.5)
    lazy val rawWeights = log(sequence(1, mu, 1)).scalarMultiply(-1).scalarAdd(logMu2)
    lazy val sumw = rawWeights.getColumn(0).sum
    lazy val sumwq = rawWeights.getColumn(0).map(x => x * x).sum

    lazy val weights = rawWeights.scalarMultiply(1 / sumw)
    lazy val mueff = sumw * sumw / sumwq
    lazy val cc = (4 + mueff / genomeSize) / (genomeSize + 4 + 2 * mueff / genomeSize)
    lazy val cs = (mueff + 2) / (genomeSize + mueff + 3.0)

    lazy val damps = (1 + 2 * FastMath.max(0, FastMath.sqrt((mueff - 1) / (genomeSize + 1)) - 1)) + cs // minor increment
    lazy val ccov1 = 2 / ((genomeSize + 1.3) * (genomeSize + 1.3) + mueff)
    lazy val ccovmu = FastMath.min(1 - ccov1, 2 * (mueff - 2 + 1 / mueff) / ((genomeSize + 2) * (genomeSize + 2) + mueff))
    lazy val chiN = FastMath.sqrt(genomeSize) * (1 - 1 / (4.0 * genomeSize) + 1 / (21.0 * genomeSize * genomeSize))

    /**
     * Update of the evolution paths ps and pc.
     *
     * @param zmean Weighted row matrix of the gaussian random numbers generating
     * the current offspring.
     * @param xold xmean matrix of the previous generation.
     * @return hsig flag indicating a small correction.
     */
    def updateEvolutionPaths(
      zmean: RealMatrix,
      xold: RealMatrix,
      xmean: RealMatrix,
      ps: RealMatrix,
      pc: RealMatrix,
      sigma: Double,
      B: RealMatrix,
      iterations: Double) = {
      val newPs = ps.scalarMultiply(1 - cs).add(B.multiply(zmean).scalarMultiply(FastMath.sqrt(cs * (2 - cs) * mueff)))
      val normps = newPs.getFrobeniusNorm
      val hsig = normps / FastMath.sqrt(1 - FastMath.pow(1 - cs, 2 * iterations)) / chiN < 1.4 + 2 / (genomeSize + 1.0);
      val pc1 = pc.scalarMultiply(1 - cc)
      val newPc =
        if (hsig) pc1.add(xmean.subtract(xold).scalarMultiply(FastMath.sqrt(cc * (2 - cc) * mueff) / sigma))
        else pc1
      (hsig, newPs, normps, newPc)
    }

    /**
     * Update of the covariance matrix C.
     *
     * @param hsig Flag indicating a small correction.
     * @param bestArx Fitness-sorted matrix of the argument vectors producing the
     * current offspring.
     * @param arz Unsorted matrix containing the gaussian random values of the
     * current offspring.
     * @param arindex Indices indicating the fitness-order of the current offspring.
     * @param xold xmean matrix of the previous generation.
     */
    def updateCovariance(
      hsig: Boolean,
      bestArx: RealMatrix,
      arzNeg: RealMatrix, // final int[] arindex,
      xold: RealMatrix,
      pc: RealMatrix,
      BD: RealMatrix,
      C: RealMatrix,
      sigma: Double) = {
      //val negccov = 0
      if (ccov1 + ccovmu > 0) {
        val arpos = bestArx.subtract(repmat(xold, 1, mu)).scalarMultiply(1 / sigma) // mu difference vectors
        val roneu = pc.multiply(pc.transpose()).scalarMultiply(ccov1) // rank one update
        // minor correction if hsig==false
        if (activeCMAES) {
          // Adapt covariance matrix C active CMA
          // keep at least 0.66 in all directions, small popsize are most
          // critical

          // prepare vectors, compute negative updating matrix Cneg
          //          final int[] arReverseIndex = reverse(arindex);
          //          RealMatrix arzneg = selectColumns(arz, MathArrays.copyOf(arReverseIndex, mu));
          //         RealMatrix arnorms = sqrt(sumRows(square(arzneg)));
          //          final int[] idxnorms = sortedIndices(arnorms.getRow(0));
          //          final RealMatrix arnormsSorted = selectColumns(arnorms, idxnorms);
          //          final int[] idxReverse = reverse(idxnorms);
          //          final RealMatrix arnormsReverse = selectColumns(arnorms, idxReverse);
          //          arnorms = divide(arnormsReverse, arnormsSorted);

          val arnormsRaw = sqrt(sumRows(square(arzNeg)))
          val idxnormsRaw = sortedIndices(arnormsRaw.getRow(0))
          val arnormsSorted = selectColumns(arnormsRaw, idxnormsRaw.toArray)
          val idxReverseRaw = reverse(idxnormsRaw.toArray)
          val arnormsReverse = selectColumns(arnormsRaw, idxReverseRaw)
          val arnorms = divide(arnormsReverse, arnormsSorted)

          // val arnormsSorted = arnormsRaw.getRow(1).sorted
          //val arnormsReverse = arnormsSorted.reverse
          //val arnorms = (arnormsReverse zip arnormsSorted).map{case (v1, v2) => v1 / v2} //divide(arnormsReverse, arnormsSorted)

          val idxnorms = sortedIndices(arnorms.getRow(0))
          val idxInv = inverse(idxnorms.toArray)
          val arnormsInv = selectColumns(arnorms, idxInv)

          // check and set learning rate negccov
          val negccovVal = (1 - ccovmu) * 0.25 * mueff / (FastMath.pow(genomeSize + 2, 1.5) + 2 * mueff)
          val negcovMax = (1 - negminresidualvariance) / square(arnormsInv).multiply(weights).getEntry(0, 0)

          val negccov = if (negccovVal > negcovMax) negcovMax else negccovVal

          //arzneg = times(arzneg, repmat(arnormsInv, dimension, 1));
          val artmp = BD.multiply(times(arzNeg, repmat(arnormsInv, genomeSize, 1)))
          val Cneg = artmp.multiply(diag(weights)).multiply(artmp.transpose())

          val oldFac = (if (hsig) 0 else ccov1 * cc * (2 - cc)) + (1 - ccov1 - ccovmu) + (negalphaold * negccov)
          val newC = C.scalarMultiply(oldFac)
            .add(roneu) // regard old matrix
            .add(arpos.scalarMultiply( // plus rank one update
              ccovmu + (1 - negalphaold) * negccov) // plus rank mu update
              .multiply(times(repmat(weights, 1, genomeSize),
                arpos.transpose)))
            .subtract(Cneg.scalarMultiply(negccov))

          (newC, negccov)
        } else {
          val oldFac = (if (hsig) 0 else ccov1 * cc * (2 - cc)) + (1 - ccov1 - ccovmu)
          val newC =
            C.scalarMultiply(oldFac)
              .add(roneu) // regard old matrix
              .add(arpos.scalarMultiply(ccovmu) // plus rank mu update
                .multiply(times(repmat(weights, 1, genomeSize),
                  arpos.transpose)))
          (newC, 0.0)
        }
      } else (C, 0.0)
    }

    /**
     * Update B and D from C.
     *
     * @param negccov Negative covariance factor.
     */
    def updateBD(negccov: Double, C: RealMatrix, iterations: Long, B: RealMatrix, D: RealMatrix, diagD: RealMatrix, BD: RealMatrix) = {
      if (ccov1 + ccovmu + negccov > 0 && (iterations % 1.0 / (ccov1 + ccovmu + negccov) / genomeSize / 10.0) < 1) {
        // to achieve O(N^2)
        val newC = triu(C, 0).add(triu(C, 1).transpose)
        // enforce symmetry to prevent complex numbers
        //(0 until newC.getRowDimension).foreach(l => println(newC.getRow(l).toSeq))
        val eig = new EigenDecomposition(newC)

        val newB = eig.getV // eigen decomposition, B==normalized eigenvectors
        val newD = eig.getD

        assert(!newB.getData.exists(_.exists(_.isNaN)))
        assert(!newD.getData.exists(_.exists(_.isNaN)))
        //val newDiagD = diag(newD)
        /*if (min(newDiagD) <= 0) {
          for (int i = 0; i < dimension; i++) {
            if (newDiagD.getEntry(i, 0) < 0) {
              newDiagD.setEntry(i, 0, 0);
            }
          }

          final double tfac = max(newDiagD) / 1e14;
          C = C.add(eye(dimension, dimension).scalarMultiply(tfac));
          newDiagD = newDiagD.add(ones(dimension, 1).scalarMultiply(tfac));
        }
        if (max(newDiagD) > 1e14 * min(newDiagD)) {
          final double tfac = max(newDiagD) / 1e14 - min(newDiagD);
          C = C.add(eye(dimension, dimension).scalarMultiply(tfac));
          newDiagD = newDiagD.add(ones(dimension, 1).scalarMultiply(tfac));
        }*/
        //(0 until newD.getRowDimension).foreach(l => println(newD.getRow(l).toSeq))

        val diagDSquare = diag(newD)
        for {
          r <- 0 until diagDSquare.getRowDimension
          c <- 0 until diagDSquare.getColumnDimension
          if diagDSquare.getEntry(r, c) < 0
        } diagDSquare.setEntry(r, c, 0)

        val newDiagD = sqrt(diagDSquare) // D contains standard deviations now

        assert(!newDiagD.getData.exists(_.exists(_.isNaN)))

        for {
          r <- 0 until newDiagD.getRowDimension
          c <- 0 until newDiagD.getColumnDimension
          if newDiagD.getEntry(r, c) < minimumSigma
        } newDiagD.setEntry(r, c, minimumSigma)

        val newBD = times(B, repmat(newDiagD.transpose, genomeSize, 1)) // O(n^2)

        (newC, newB, newD, newDiagD, newBD)
      } else (C, B, D, diagD, BD)
    }

    val fitness = population.map(i => aggregate(i.fitness))
    val sortedFiteness = fitness.sorted
    // Sort by fitness and compute weighted mean into xmean
    val arindex = fitness.zipWithIndex.sortBy { case (v, _) => v }.map(_._2).toArray //val arindex: Array[Int] = sortedIndices(fitness)

    // Calculate new xmean, this is selection and recombination
    val xold = a.xmean //val xold: RealMatrix = xmean

    val sortedOffspring = population.sortBy(i => aggregate(i.fitness))

    val bestGenomes = sortedOffspring.take(mu).map { i => i.genome |-> values get }
    val bestArx: RealMatrix = new Array2DRowRealMatrix(bestGenomes.transpose.map(_.toArray).toArray, false) //selectColumns(arx, MathArrays.copyOf(arindex, mu))

    val sortedRandomValues = sortedOffspring.map { i => i.genome |-> randomValues get }
    //val arz: RealMatrix = new Array2DRowRealMatrix(sigmas.map(_.toArray).toArray, false)
    val bestArz: RealMatrix = new Array2DRowRealMatrix(sortedRandomValues.take(mu).transpose.map(_.toArray).toArray, false) //selectColumns(arz, MathArrays.copyOf(arindex, mu));

    val xmean = bestArx.multiply(weights)
    val zmean = bestArz.multiply(weights)

    val (hsig, newPs, newNormps, newPc) =
      updateEvolutionPaths(
        zmean = zmean,
        xold = xold,
        xmean = xmean,
        ps = a.ps,
        pc = a.pc,
        sigma = a.sigma,
        B = a.B,
        iterations = a.iterations)

    //if (diagonalOnly <= 0) {
    val arzNeg = new Array2DRowRealMatrix(sortedRandomValues.reverse.take(mu).transpose.map(_.toArray).toArray, false)
    val (matC, negccov) = updateCovariance(hsig, bestArx, arzNeg, xold, newPc, a.BD, a.C, a.sigma)
    val (newC, newB, newD, newDiagD, newBD) = updateBD(negccov, matC, a.iterations, a.B, a.D, a.diagD, a.BD)
    val newDiagC = diag(newC)

    /*} else {
      updateCovarianceDiagonalOnly(hsig, bestArz);
    }*/
    // Adapt step size sigma - Eq. (5)
    val bestFitness = sortedFiteness.head
    val newBestValue = FastMath.min(bestFitness, a.bestValue)

    val nS1 = a.sigma * FastMath.exp(FastMath.min(1, (newNormps / chiN - 1) * cs / damps))

    val nS2 =
      if (newBestValue == sortedFiteness((0.1 + lambda / 4.0).toInt))
        nS1 * FastMath.exp(0.2 + cs / damps)
      else nS1

    val historyWorst = a.fitnessHistory.content.max
    val historyBest = a.fitnessHistory.content.min

    val newSigma =
      if (a.iterations > 1 && FastMath.max(historyWorst, bestFitness) - FastMath.min(historyBest, bestFitness) == 0)
        nS2 * FastMath.exp(0.2 + cs / damps)
      else nS2

    val historySize = 10 + (3 * 10 * genomeSize / lambda.toDouble).toInt
    val fitnessHistory = a.fitnessHistory += (bestFitness, historySize)

    CMAESArchive.Archive(
      xmean = xmean,
      B = newB,
      D = newD,
      C = newC,
      BD = newBD,
      diagC = newDiagC,
      diagD = newDiagD,
      sigma = newSigma,
      ps = newPs,
      pc = newPc,
      normps = newNormps,
      fitnessHistory = fitnessHistory,
      bestValue = newBestValue,
      iterations = a.iterations + 1)

    // store best in history
    //push(fitnessHistory,bestFitness)

    /*final double bestFitness = fitness[arindex[0]];
    final double worstFitness = fitness[arindex[arindex.length - 1]];
    if (bestValue > bestFitness) {
      bestValue = bestFitness;
      lastResult = optimum;
      optimum = new PointValuePair(fitfun.repair(bestArx.getColumn(0)),
        isMinimize ? bestFitness : -bestFitness);
      if (getConvergenceChecker() != null && lastResult != null &&
        getConvergenceChecker().converged(iterations, optimum, lastResult)) {
        break generationLoop;
      }
    }
    // handle termination criteria
    // Break, if fitness is good enough
    if (stopFitness != 0 && bestFitness < (isMinimize ? stopFitness : -stopFitness)) {
      break generationLoop;
    }
    final double[] sqrtDiagC = sqrt(diagC).getColumn(0);
    final double[] pcCol = pc.getColumn(0);
    for (int i = 0; i < dimension; i++) {
      if (sigma * FastMath.max(FastMath.abs(pcCol[i]), sqrtDiagC[i]) > stopTolX) {
        break;
      }
      if (i >= dimension - 1) {
        break generationLoop;
      }
    }
    for (int i = 0; i < dimension; i++) {
      if (sigma * sqrtDiagC[i] > stopTolUpX) {
        break generationLoop;
      }
    }
    final double historyBest = min(fitnessHistory);
    final double historyWorst = max(fitnessHistory);
    if (iterations > 2 &&
      FastMath.max(historyWorst, worstFitness) -
        FastMath.min(historyBest, bestFitness) < stopTolFun) {
      break generationLoop;
    }
    if (iterations > fitnessHistory.length &&
      historyWorst - historyBest < stopTolHistFun) {
      break generationLoop;
    }
    // condition number of the covariance matrix exceeds 1e14
    if (max(diagD) / min(diagD) > 1e7) {
      break generationLoop;
    }
    // user defined termination
    if (getConvergenceChecker() != null) {
      final PointValuePair current
        = new PointValuePair(bestArx.getColumn(0),
        isMinimize ? bestFitness : -bestFitness);
      if (lastResult != null &&
        getConvergenceChecker().converged(iterations, current, lastResult)) {
        break generationLoop;
      }
      lastResult = current;
    }
    // Adjust step size in case of equal function values (flat fitness)
    if (bestValue == fitness[arindex[(int)(0.1+lambda/4.)]]) {
      sigma *= FastMath.exp(0.2 + cs / damps);
    }
    if (iterations > 2 && FastMath.max(historyWorst, bestFitness) -
      FastMath.min(historyBest, bestFitness) == 0) {
      sigma *= FastMath.exp(0.2 + cs / damps);
    }
    // store best in history
    push(fitnessHistory,bestFitness);
    if (generateStatistics) {
      statisticsSigmaHistory.add(sigma);
      statisticsFitnessHistory.add(bestFitness);
      statisticsMeanHistory.add(xmean.transpose());
      statisticsDHistory.add(diagD.transpose().scalarMultiply(1E5));
    }*/
  }

  /**
   * @param m Input matrix.
   * @param cols Columns to select.
   * @return Matrix representing the selected columns.
   */
  private def selectColumns(m: RealMatrix, cols: Array[Int]) = {
    val d = Array.tabulate[Double](m.getRowDimension, cols.length) {
      (r, c) => m.getEntry(r, cols(c))
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix
   * @return Matrix representing the element-wise logarithm of m.
   */
  private def log(m: RealMatrix) = {
    val d = Array.tabulate[Double](m.getRowDimension, m.getColumnDimension) {
      (r, c) => FastMath.log(m.getEntry(r, c));
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix.
   * @return Matrix representing the element-wise square of m.
   */
  private def square(m: RealMatrix) = {
    val d = Array.tabulate[Double](m.getRowDimension, m.getColumnDimension) {
      (r, c) =>
        val e = m.getEntry(r, c)
        e * e
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param n Number of rows.
   * @param m Number of columns.
   * @return n-by-m matrix of zero values.
   */
  private def zeros(n: Int, m: Int) = new Array2DRowRealMatrix(n, m)

  /**
   * @param n Number of rows.
   * @param m Number of columns.
   * @return n-by-m matrix filled with 1.
   */
  private def ones(n: Int, m: Int) = {
    val d = Array.fill(n, m)(1.0)
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param n Number of rows.
   * @param m Number of columns.
   * @return n-by-m matrix of 0 values out of diagonal, and 1 values on
   * the diagonal.
   */
  private def eye(n: Int, m: Int) = {
    val d = Array.ofDim[Double](n, m)
    for {
      r <- 0 until math.min(n, m)
    } d(r)(r) = 1
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix 1.
   * @param n Input matrix 2.
   * @return the matrix where the elements of m and n are element-wise multiplied.
   */
  private def times(m: RealMatrix, n: RealMatrix) = {
    val d = Array.tabulate(m.getRowDimension, m.getColumnDimension) {
      (r, c) => m.getEntry(r, c) * n.getEntry(r, c)
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param mat Input matrix.
   * @param n Number of row replicates.
   * @param m Number of column replicates.
   * @return a matrix which replicates the input matrix in both directions.
   */
  private def repmat(mat: RealMatrix, n: Int, m: Int) = {
    val rd = mat.getRowDimension
    val cd = mat.getColumnDimension
    val d = Array.tabulate(n * rd, m * cd) {
      (r, c) => mat.getEntry(r % rd, c % cd)
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix.
   * @return the diagonal n-by-n matrix if m is a column matrix or the column
   * matrix representing the diagonal if m is a n-by-n matrix.
   */
  private def diag(m: RealMatrix) = {
    if (m.getColumnDimension == 1) {
      val d = Array.ofDim[Double](m.getRowDimension, m.getRowDimension)
      for (i <- 0 until m.getRowDimension) d(i)(i) = m.getEntry(i, 0)
      new Array2DRowRealMatrix(d, false)
    } else {
      val d = Array.tabulate[Double](m.getRowDimension, 1) {
        (i, _) => m.getEntry(i, i)
      }
      new Array2DRowRealMatrix(d, false)
    }
  }

  /**
   * @param m Input matrix.
   * @return Row matrix representing the sums of the rows.
   */
  private def sumRows(m: RealMatrix) = {
    val d =
      Array(
        Array.tabulate[Double](m.getColumnDimension) {
          c =>
            var sum = 0.0
            for (r <- 0 until m.getRowDimension) sum += m.getEntry(r, c)
            sum
        }
      )
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix.
   * @return Matrix representing the element-wise square root of m.
   */
  private def sqrt(m: RealMatrix) = {
    val d = Array.tabulate[Double](m.getRowDimension, m.getColumnDimension) {
      (r, c) => FastMath.sqrt(m.getEntry(r, c))
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param m Input matrix 1.
   * @param n Input matrix 2.
   * @return Matrix where the elements of m and n are element-wise divided.
   */
  private def divide(m: RealMatrix, n: RealMatrix) = {
    val d = Array.tabulate[Double](m.getRowDimension, m.getColumnDimension) {
      (r, c) => m.getEntry(r, c) / n.getEntry(r, c)
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * Sorts fitness values.
   *
   * @param doubles Array of values to be sorted.
   * @return a sorted array of indices pointing into doubles.
   */
  private def sortedIndices(doubles: Array[Double]) = doubles.zipWithIndex.sortBy { case (v, _) => v }.unzip._2

  /**
   * @param indices Input index array.
   * @return the inverse of the mapping defined by indices.
   */
  private def inverse(indices: Array[Int]) = {
    val inverse = Array.ofDim[Int](indices.length)
    for (i <- 0 until indices.length) inverse(indices(i)) = i;
    inverse
  }

  /**
   * @param indices Input index array.
   * @return the indices in inverse order (last is first).
   */
  private def reverse(indices: Array[Int]) = indices.reverse

  /**
   * @param m Input matrix.
   * @param k Diagonal position.
   * @return Upper triangular part of matrix.
   */
  private def triu(m: RealMatrix, k: Int) = {
    val d = Array.tabulate[Double](m.getRowDimension, m.getColumnDimension) {
      (r, c) => if (r <= c - k) m.getEntry(r, c) else 0
    }
    new Array2DRowRealMatrix(d, false)
  }

  /**
   * @param start Start value.
   * @param end End value.
   * @param step Step size.
   * @return a sequence as column matrix.
   */
  private def sequence(start: Double, end: Double, step: Double) = {
    val size = ((end - start) / step + 1).toInt
    val d = Array.ofDim[Double](size, 1)
    var value = start
    for (r <- 0 until size) {
      d(r)(0) = value
      value += step
    }
    new Array2DRowRealMatrix(d, false)
  }
}

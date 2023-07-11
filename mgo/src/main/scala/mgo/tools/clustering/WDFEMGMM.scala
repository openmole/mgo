package mgo.tools.clustering

import better.files.File
import breeze.linalg.Matrix.*
import breeze.linalg.Vector.*
import breeze.linalg.*
import breeze.numerics.*
import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import org.apache.commons.math3.distribution.{ MixtureMultivariateNormalDistribution, MultivariateNormalDistribution, NormalDistribution }
import org.apache.commons.math3.linear.{ CholeskyDecomposition, NonPositiveDefiniteMatrixException }
import org.apache.commons.math3.util.Pair

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.Random
import util.{ Failure, Success, Try }
import mgo.tools.Breeze

/**
 * Weighted-data Gaussian mixture model (WDF-GMM) with Fixed weight implementation.
 * https://team.inria.fr/perception/research/wdgmm/
 * Gebru, I. D., Alameda-Pineda, X., Forbes, F., & Horaud, R. (2016). EM Algorithms for Weighted-Data Clustering with Application to Audio-Visual Scene Analysis. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(12), 2402–2415. doi:10.1109/tpami.2016.2522425
 */
object WDFEMGMM {

  object Clustering {

    def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
      val q = DenseMatrix.tabulate(x.cols, x.cols)((j, k) => Array.tabulate(x.rows)(i => (x(i, j) - mean(j)) * (x(i, k) - mean(k))).sum)
      (q /:/ (x.rows - 1).toDouble).toDenseMatrix
    }

    def build(x: Array[Array[Double]], dataWeights: Array[Double], minPoints: Int): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) = {
      val pointSize = x.head.length

      def computeCentroid(points: Array[Array[Double]], weights: Array[Double]) = {
        def average(x: Array[Double], w: Array[Double]) = (x zip w).map { case (x, w) => x * w }.sum / w.sum
        points.transpose.map { coord => average(coord, weights) }
      }

      def buildSingleCluster(): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) = {
        val centroids = computeCentroid(x, dataWeights.toArray)
        val weight = Array(1.0)
        val covariance = {
          val clusterMatrix = Breeze.arrayToDenseMatrix(x)
          val centroidVector = new DenseVector(centroids)
          Breeze.matrixToArray(cov(clusterMatrix, centroidVector))
        }
        (Array(centroids), Array(covariance), weight)
      }

      import jsat.clustering._
      import jsat.clustering.kmeans._
      import jsat.linear.distancemetrics._

      val hdbScan = new HDBSCAN
      hdbScan.setMinPoints(minPoints)

      if (x.size <= hdbScan.getMinPoints) buildSingleCluster()
      else {
        val dataSet = {
          val dataPoints = (x zip dataWeights).map {
            case (p, w) =>
              new DataPoint(new jsat.linear.DenseVector(p), w)
          }
          new SimpleDataSet(dataPoints.toList.asJava)
        }

        val clusters = hdbScan.cluster(dataSet).asScala.map(_.asScala.toArray).toArray

        if (!clusters.isEmpty) {
          val centroids =
            clusters.map { cluster =>
              val points = cluster.map(_.getNumericalValues.arrayCopy())
              val weights = cluster.map(_.getWeight)
              computeCentroid(points, weights)
            }

          val totalWeight = clusters.flatten.map(_.getWeight).sum
          val weights = clusters.map(_.map(_.getWeight).sum / totalWeight)

          val covariances = (clusters zip centroids).map {
            case (cluster, centroid) =>
              val clusterMatrix = Breeze.arrayToDenseMatrix(cluster.map(c => c.getNumericalValues.arrayCopy()))
              val centroidVector = new DenseVector(centroid)
              cov(clusterMatrix, centroidVector)
          }

          (centroids, covariances.map(Breeze.matrixToArray), weights)
        } else buildSingleCluster()

      }
    }

  }

  /**
   * Full covariance Gaussian Mixture Model, trained using Expectation Maximization.
   *
   * @param x data points
   */
  def initializeAndFit(
    iterations: Int,
    tolerance: Double,
    x: Array[Array[Double]],
    dataWeights: Option[Array[Double]] = None,
    minClusterSize: Int,
    random: Random): Try[(GMM, Seq[Double])] = {
    def dataWeigthsValue = dataWeights.getOrElse(x.map(_ => 1.0))

    // initialize parameters using KMeans
    val (means, covariances, weights) = Clustering.build(x, dataWeigthsValue, minClusterSize)

    assert(covariances.forall(_.forall(_.forall(!_.isNaN))), s"covariances with nan: ${covariances.mkString("\n")}")

    fit(
      x = x,
      dataWeights = dataWeigthsValue,
      gmm = GMM(means = means, covariances = covariances, weights = weights),
      iterations = iterations,
      tolerance = tolerance,
      trace = IndexedSeq())
  }

  def toDistribution(gmm: GMM, random: Random): MixtureMultivariateNormalDistribution = {
    import org.apache.commons.math3.distribution._
    import org.apache.commons.math3.util._

    import scala.jdk.CollectionConverters._

    def dist = (gmm.means zip gmm.covariances).map { case (m, c) => new MultivariateNormalDistribution(m, c) }
    def pairs = (dist zip gmm.weights).map { case (d, w) => new Pair(java.lang.Double.valueOf(w), d) }.toList

    assert(pairs.nonEmpty, s"Empty pairs for size ${gmm.means.size}")
    new MixtureMultivariateNormalDistribution(
      mgo.tools.apacheRandom(random),
      pairs.asJava)
  }

  @tailrec
  def fit(
    x: Array[Array[Double]],
    dataWeights: Array[Double],
    gmm: GMM,
    iterations: Int,
    tolerance: Double,
    logLikelihood: Double = 0.0,
    trace: Seq[Double] = Seq()): Try[(GMM, Seq[Double])] = {

    iterations match {
      case 0 => Success((gmm, trace))
      case i =>
        eStep(x, dataWeights, gmm.means, gmm.covariances, gmm.weights) match {
          case Success((updatedLogLikelihood, resp)) =>
            val updatedGMM = mStep(x, dataWeights, resp, gmm.components)
            if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) Success((gmm, trace :+ updatedLogLikelihood))
            else fit(
              x = x,
              dataWeights = dataWeights,
              gmm = updatedGMM,
              logLikelihood = updatedLogLikelihood,
              iterations = i - 1,
              tolerance = tolerance,
              trace = trace :+ updatedLogLikelihood)
          case Failure(e) => Failure(e)
        }
    }
  }

  /**
   * Estimate a covariance matrix, given data.
   * @param x data points
   */
  def cov(x: DenseMatrix[Double]): DenseMatrix[Double] = {
    val mean = sum(x(::, *)) /:/ x.rows.toDouble
    val tmp = DenseMatrix.tabulate(x.rows, x.cols)((_, j) => mean(j))
    val m = (x - tmp).toDenseMatrix
    val p = m.t * m
    (p /:/ (x.rows - 1).toDouble).toDenseMatrix
  }
  /**
   * Estimate a covariance matrix, given data.
   * @param x data points
   */
  def cov(x: DenseMatrix[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val q = DenseMatrix.tabulate(x.cols, x.cols)((j, k) => Array.tabulate(x.rows)(i => (x(i, j) - mean(j)) * (x(i, k) - mean(k))).sum)
    (q /:/ (x.rows - 1).toDouble).toDenseMatrix
  }
  /**
   * E-step: compute responsibilities,
   * update resp matrix so that resp[j, k] is the responsibility of cluster k for data point j,
   * to compute likelihood of seeing data point j given cluster k.
   *
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def eStep(
    x: Array[Array[Double]],
    dataWeights: Array[Double],
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double]): Try[(Double, Array[Array[Double]])] = {
    // for each point and each component
    // the matrix containing the probability of point i for component k multiplied by the weight (coefficient) of component k
    assert(weights.forall(p => p <= 1.0 && p >= 0), s"weights=${weights}")
    //assert(dataWeights.forall(p=> p >= 1.0), s"dataweights=${dataWeights}")
    //assert(x.rows>10,s"data=$x")
    compute_log_likelihood(Breeze.arrayToDenseMatrix(x), Breeze.arrayToDenseVector(dataWeights), Breeze.arrayToDenseMatrix(means), covariances.map(Breeze.arrayToDenseMatrix), Breeze.arrayToDenseVector(weights)) map { resp =>
      //assert(resp.forall(p=> p > 0), s"RESP=${resp}")
      // for each point, the sum of all likelihoods for all components
      val resp_sum = sum(resp(*, ::))
      //println(s"resp_sum=$resp_sum")
      val log_likelihood = sum(log(resp_sum))
      // divide the responsibility by the sum for each point
      val updatedResp = Array.tabulate(resp.rows, resp.cols)((i, j) => resp(i, j) / (if (resp_sum(i) == 0) 1.0 else resp_sum(i)))
      //assert(updatedResp.forall(_.forall(p=> p <= 1.0 && p >= 0)),s"UPDATED_RESP (${updatedResp.rows},${updatedResp.cols}) =${updatedResp}")
      // assert(sum(updatedResp(*, ::)).forall(p=> p==1.0),s"sums=${sum(updatedResp(*, ::))}")
      (log_likelihood, updatedResp)
    }
  }

  def toDenseMatrix(rows: Int, cols: Int, array: Array[Array[Double]]): DenseMatrix[Double] = {
    // we need to transpose the array first because of breeze column first representation of matrices
    DenseMatrix.create(rows, cols, array.transpose.flatten)
  }

  /**
   * Compute the log likelihood (used for e step).
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def compute_log_likelihood(
    x: DenseMatrix[Double],
    dataWeights: DenseVector[Double],
    means: DenseMatrix[Double],
    covariances: Array[DenseMatrix[Double]],
    weights: DenseVector[Double]): Try[DenseMatrix[Double]] = Try {
    import org.apache.commons.math3.linear.{ Array2DRowRealMatrix, EigenDecomposition }
    import org.apache.commons.math3.util.FastMath

    DenseMatrix.tabulate(x.rows, weights.length) { (i, k) =>
      val weightedCovariances = covariances(k) /:/ dataWeights(i)
      val determinant = det(weightedCovariances)
      val mMeans = means(k, ::).inner.toArray
      val dimension = mMeans.size
      val covarianceArray = Breeze.matrixToArray(weightedCovariances)

      def density(vals: Array[Double], cholesky: Boolean = false) = {
        def covarianceMatrixInverse = {
          val covarianceMatrix = new Array2DRowRealMatrix(covarianceArray)
          val covMatDec = new CholeskyDecomposition(covarianceMatrix)
          covMatDec.getSolver.getInverse
        }

        def getExponentTerm(values: Array[Double]) = {
          val centered = Array.tabulate(values.length) { i => values(i) - mMeans(i) }
          val preMultiplied = covarianceMatrixInverse.preMultiply(centered)

          var sum: Double = 0
          for (i <- 0 until preMultiplied.length) {
            sum += preMultiplied(i) * centered(i)
          }

          FastMath.exp(-0.5 * sum)
        }

        FastMath.pow(2 * FastMath.PI, -0.5 * dimension) * FastMath.pow(determinant, -0.5) * getExponentTerm(vals)
      }

      density(x(i, ::).inner.toArray, cholesky = true) * weights(k)
    }
  }

  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(x: Array[Array[Double]], dataWeights: Array[Double], resp: Array[Array[Double]], components: Int): GMM = {
    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val xMatrix = Breeze.arrayToDenseMatrix(x)
    val resp_t = Breeze.arrayToDenseMatrix(resp).t
    val component_weights = sum(resp_t(*, ::))
    // normalized weights (mixture coefficients)
    val weights = component_weights /:/ xMatrix.rows.toDouble
    // means
    // for all components : the sum of the product of responsibility by point values weighted by point weight
    val weighted_sum = resp_t * DenseMatrix.tabulate(xMatrix.rows, xMatrix.cols)((i, j) => xMatrix(i, j) * dataWeights(i))
    // for all components : the sum of the product of responsibility by point weight
    val weighted_resp = resp_t * Breeze.arrayToDenseVector(dataWeights).toDenseMatrix.t
    val means = DenseMatrix.tabulate(weighted_sum.rows, weighted_sum.cols)((i, j) => weighted_sum(i, j) / weighted_resp(i, 0))
    // covariance
    //    println(s"components = $components")

    val covariances = Array.tabulate(components) { k =>
      val mean = means(k, ::)
      val w_sum = DenseMatrix.tabulate(xMatrix.cols, xMatrix.cols) {
        (covRow, covCol) => Array.tabulate(xMatrix.rows) { i => (xMatrix(i, covRow) - mean(covRow)) * (xMatrix(i, covCol) - mean(covCol)) * resp_t(k, i) * dataWeights(i) }.sum
      }
      w_sum /:/ component_weights(k)
    }
    GMM(weights = Breeze.vectorToArray(weights), means = Breeze.matrixToArray(means), covariances = covariances.map(Breeze.matrixToArray))
  }

  /**
   * 2d matrix dot product.
   * @param A matrix A
   * @param B matrix B
   */
  def dot(A: Array[Array[Double]], B: Array[Array[Double]]): Array[Array[Double]] = {
    Array.tabulate(A.length)(i => B.indices.map(j => B(j).map(_ * A(i)(j))).transpose.map(_.sum).toArray)
  }

  def dilate(gmm: GMM, f: Double): GMM =
    gmm.copy(covariances = gmm.covariances.map(_.map(_.map(_ * f))))

}

package mgo.tools.clustering

import better.files.File
import org.apache.commons.math3.distribution.{ MixtureMultivariateNormalDistribution, MultivariateNormalDistribution }

import scala.annotation.tailrec
import scala.util.Random

/**
 * EM-GMM implementation.
 * Inspired by the work of Maël Fabien: https://github.com/maelfabien/EM_GMM_HMM
 */
object EMGMM {

  /**
   * Full covariance Gaussian Mixture Model, trained using Expectation Maximization.
   *
   * @param x data points
   * @param columns number of data columns
   */
  def initializeAndFit(
    components: Int,
    iterations: Int,
    tolerance: Double,
    x: Array[Array[Double]],
    columns: Int,
    random: Random): (GMM, Seq[Double]) = {
    // initialize parameters
    // chose Random means in data points
    val means = random.shuffle(x.indices.toArray[Int]).take(components).map(c => x(c)).toArray
    // set equal weights to all components
    val weights = Array.fill(components)(1.0 / components)
    // compute covariances
    val covariances = Array.fill(components)(cov(x, columns))
    val (gmm, logLikelihoodTrace) =
      fit(
        x = x,
        means = means,
        covariances = covariances,
        weights = weights,
        components = components,
        iterations = iterations,
        tolerance = tolerance,
        trace = IndexedSeq())

    (gmm, logLikelihoodTrace)
  }

  def toDistribution(gmm: GMM, random: Random): MixtureMultivariateNormalDistribution = {
    import org.apache.commons.math3.distribution._
    import org.apache.commons.math3.util._

    import scala.jdk.CollectionConverters._

    def dist = (gmm.means zip gmm.covariances).map { case (m, c) => new MultivariateNormalDistribution(m, c) }
    def pairs = (dist zip gmm.weights).map { case (d, w) => new Pair(java.lang.Double.valueOf(w), d) }.toList

    new MixtureMultivariateNormalDistribution(mgo.tools.apacheRandom(random), pairs.asJava)
  }

  @tailrec
  def fit(
    x: Array[Array[Double]],
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    components: Int,
    iterations: Int,
    tolerance: Double,
    logLikelihood: Double = 0.0,
    trace: Seq[Double] = Seq()): (GMM, Seq[Double]) = {
    def gmm =
      GMM(
        means = means,
        covariances = covariances,
        weights = weights)

    iterations match {
      case 0 => (gmm, trace)
      case i =>
        val (updatedLogLikelihood, resp) = eStep(x, means, covariances, weights)
        val (updatedWeights, updatedMeans, updatedCovariances) = mStep(x, resp, components)
        if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) (gmm, trace :+ updatedLogLikelihood)
        else fit(
          x = x,
          means = updatedMeans,
          covariances = updatedCovariances,
          weights = updatedWeights,
          logLikelihood = updatedLogLikelihood,
          components = components,
          iterations = i - 1,
          tolerance = tolerance,
          trace = trace :+ updatedLogLikelihood)
    }
  }

  /**
   * Estimate a covariance matrix, given data.
   * @param x data points
   * @param columns number of columns of the points
   */
  def cov(x: Array[Array[Double]], columns: Int): Array[Array[Double]] = {
    val mean = Array.tabulate(columns) { i => x.map(_(i)).sum / x.length }
    val m = x.map(_.zipWithIndex.map(v => v._1 - mean(v._2)))
    val p = m.map(v => v.map(x => v.map(_ * x)))
    Array.tabulate(columns, columns)((i, j) => p.map(_(i)(j)).sum / (x.length - 1))
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
  def eStep(x: Array[Array[Double]], means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]], weights: Array[Double]): (Double, Array[Array[Double]]) = {
    // resp matrix
    val resp = compute_log_likelihood(x, means, covariances, weights)
    val sum = resp.map(_.sum)
    val log_likelihood = sum.map(math.log).sum
    val updatedResp = resp.zip(sum).map { case (v, div) => v.map(_ / div) }
    (log_likelihood, updatedResp)
  }

  /**
   * Compute the log likelihood (used for e step).
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def compute_log_likelihood(x: Array[Array[Double]], means: Array[Array[Double]], covariances: Array[Array[Array[Double]]], weights: Array[Double]): Array[Array[Double]] = {
    weights.zipWithIndex.map { case (prior, k) => x.map(x => new MultivariateNormalDistribution(means(k), covariances(k)).density(x) * prior) }.transpose
  }

  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(X: Array[Array[Double]], resp: Array[Array[Double]], components: Int): (Array[Double], Array[Array[Double]], Array[Array[Array[Double]]]) = {
    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val resp_weights = Array.tabulate(components)(i => resp.map(_(i)).sum)
    // normalized weights
    val weights = resp_weights.map(_ / X.length)
    // means
    val weighted_sum = dot(resp.transpose, X)
    val means = weighted_sum.zip(resp_weights).map { case (array, w) => array.map(_ / w) }
    // covariance
    val resp_t = resp.transpose
    val covariances = Array.tabulate(components) { k =>
      val diff = X.map(x => x.indices.map(i => x(i) - means(k)(i)).toArray).transpose
      val resp_k = resp_t(k)
      val w_sum = dot(diff.map { l => l.zip(resp_k).map { case (a, b) => a * b } }, diff.transpose)
      w_sum.map(_.map(_ / resp_weights(k)))
    }
    (weights, means, covariances)
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

//object GMM {
//
//  import ppse.tool.Display._
//
//  def toString(gmm: GMM): String = {
//    s"${arrayToString(gmm.weights)},${arrayToString(gmm.means)},${arrayToString(gmm.covariances)}"
//  }
//
//}

case class GMM(
  means: Array[Array[Double]],
  covariances: Array[Array[Array[Double]]],
  weights: Array[Double]) {
  def components = means.size
}


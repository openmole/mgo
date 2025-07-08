package mgo.tools.clustering


import org.apache.commons.math3.distribution.{MixtureMultivariateNormalDistribution, MultivariateNormalDistribution}
import org.apache.commons.math3.stat.correlation.Covariance

import scala.annotation.tailrec
import scala.util.Random
import scala.util.{Try, Success, Failure}

/**
 * EM-GMM implementation.
 * Inspired by the work of Maël Fabien: https://github.com/maelfabien/EM_GMM_HMM
 */
object EMGMM:
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
    regularisationEpsilon: Double,
    x: Array[Array[Double]],
    columns: Int,
    random: Random): (GMM, Seq[Double]) =

    def covariance(x: Array[Array[Double]]) = new Covariance(x).getCovarianceMatrix.getData

    // initialize parameters
    // chose Random means in data points
    val means = random.shuffle(x.indices.toArray[Int]).take(components).map(c => x(c)).toArray
    // set equal weights to all components
    val weights = Array.fill(components)(1.0 / components)
    // compute covariances
    val covariances = Array.fill(components)(covariance(x))

    val (gmm, logLikelihoodTrace) =
      EMGMM.fit(
        x = x,
        means = means,
        covariances = covariances,
        weights = weights,
        components = components,
        iterations = iterations,
        tolerance = tolerance,
        regularisationEpsilon = regularisationEpsilon,
        trace = IndexedSeq()
      )

    (gmm, logLikelihoodTrace)

  @tailrec
  def fit(
    x: Array[Array[Double]],
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    components: Int,
    iterations: Int,
    tolerance: Double,
    regularisationEpsilon: Double,
    logLikelihood: Double = 0.0,
    trace: Seq[Double] = Seq()): (GMM, Seq[Double]) =
    def gmm = GMM(means, covariances, weights)

    iterations match
      case 0 => (gmm, trace)
      case i =>
        val (updatedLogLikelihood, resp) = eStep(x, means, covariances, weights, regularisationEpsilon)
        val (updatedWeights, updatedMeans, updatedCovariances) = mStep(x, resp, components, regularisationEpsilon)
        if (math.abs(updatedLogLikelihood - logLikelihood) <= tolerance) (gmm, trace :+ updatedLogLikelihood)
        else fit(
          x = x,
          means = updatedMeans,
          covariances = updatedCovariances,
          weights = updatedWeights,
          logLikelihood = updatedLogLikelihood,
          regularisationEpsilon = regularisationEpsilon,
          components = components,
          iterations = i - 1,
          tolerance = tolerance,
          trace = trace :+ updatedLogLikelihood)


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
    means: Array[Array[Double]],
    covariances: Array[Array[Array[Double]]],
    weights: Array[Double],
    regularisationEpsilon: Double): (Double, Array[Array[Double]]) =
    // resp matrix
    val resp = compute_log_likelihood(x, means, covariances, weights, regularisationEpsilon)
    assert(resp.flatten.forall(!_.isNaN))

    val sum = resp.map(_.sum)
    val log_likelihood = sum.map(math.log).sum
    assert(!log_likelihood.isNaN)

    val updatedResp =
      resp.zip(sum).map: (v, div) =>
        v.map: x =>
          if div != 0.0 then x / div else 0.0

    (log_likelihood, updatedResp)

  /**
   * Compute the log likelihood (used for e step).
   * @param x data points
   * @param means means of the components (clusters)
   * @param covariances covariances of the components (clusters)
   * @param weights weights of the components (clusters)
   */
  def compute_log_likelihood(x: Array[Array[Double]], means: Array[Array[Double]], covariances: Array[Array[Array[Double]]], weights: Array[Double], regularisationEpsilon: Double): Array[Array[Double]] =
    val res =
      weights.zipWithIndex.map: (prior, k) =>
        val distributionTry = Try(new MultivariateNormalDistribution(means(k), covariances(k)))
        val distribution =
          distributionTry match
            case Success(v) => v
            case Failure(e) =>
              new MultivariateNormalDistribution(means(k), regularize(covariances(k), regularisationEpsilon))

        x.map: x =>
          distribution.density(x) * prior

    res.transpose

  /**
   * Regularize the matrix by adding a certain value to the diagonal.
   * @param matrix input matrix
   * @param v value to add to the diagonal
   * @return a regularized matrix
   */
  def regularize(matrix: Array[Array[Double]], v: Double): Array[Array[Double]] =
    matrix.zipWithIndex.map: (array, i) =>
      array.zipWithIndex.map: (value, j) =>
        if i == j then value + v else value

  def punctualGMM(x: Array[Array[Double]], regularisationEpsilon: Double): GMM =
    if x.isEmpty
    then GMM.empty
    else
      val size = x.head.length
      val cov =
        Array.tabulate(size, size): (i, j) =>
          if i == j then regularisationEpsilon else 0.0

      val components =
        x.map: x =>
          GMM.Component(x, cov, 1)

      GMM(components)


  /**
   * M-step, update parameters.
   * @param X data points
   */
  def mStep(X: Array[Array[Double]], resp: Array[Array[Double]], components: Int, epsilon: Double): (Array[Double], Array[Array[Double]], Array[Array[Array[Double]]]) =
    // sum the columns to get total responsibility assigned to each cluster, N^{soft}
    val resp_weights = Array.tabulate(components)(i => resp.map(_ (i)).sum)
    // normalized weights
    val weights = resp_weights.map(_ / X.length)
    // means
    val weighted_sum = dot(resp.transpose, X)
    val means = weighted_sum.zip(resp_weights).map { case (array, w) => array.map(_ / w) }


    // covariance
    val resp_t = resp.transpose
    val covariances = Array.tabulate(components): k =>
      val diff = X.map(x => x.indices.map(i => x(i) - means(k)(i)).toArray).transpose
      val resp_k = resp_t(k)
      val w_sum = dot(diff.map { l => l.zip(resp_k).map {case (a, b) => a * b }}, diff.transpose)
      regularize(w_sum.map(_.map(_ / resp_weights(k))), epsilon)

    assert(resp.flatten.forall(!_.isNaN))
    assert(means.flatten.forall(!_.isNaN))
    assert(resp_weights.forall(!_.isNaN))
    assert(covariances.flatten.flatten.forall(!_.isNaN))


    (weights, means, covariances)

  def integrateOutliers(x: Array[Array[Double]], gmm: GMM, regularisationEpsilon: Double) =
    if GMM.isEmpty(gmm)
    then punctualGMM(x, regularisationEpsilon)
    else
      val (_, resp) = EMGMM.eStep(x, gmm.means, gmm.covariances, gmm.weights, regularisationEpsilon)

      val excludedIndex = resp.zipWithIndex.filter((r, _) => r.sum == 0.0).map(_._2).zipWithIndex.toMap
      val excluded = excludedIndex.size

      def newResp =
        for
          (r, i) <- resp.zipWithIndex
        yield
          val excludedWeight =
            excludedIndex.get(i) match
              case None => Vector.fill(excluded)(0.0)
              case Some(index) => Vector.tabulate(excluded)(i => if i == index then 1.0 else 0.0)

          r ++ excludedWeight

      val (w, m, c) = EMGMM.mStep(x, newResp, gmm.size + excluded, regularisationEpsilon)
      GMM(m, c, w)

  /**
   * 2d matrix dot product.
   * @param A matrix A
   * @param B matrix B
   */
  def dot(A: Array[Array[Double]], B: Array[Array[Double]]): Array[Array[Double]] =
    Array.tabulate(A.length)(i=>B.indices.map(j=>B(j).map(_*A(i)(j))).transpose.map(_.sum).toArray)

object GMM:
  def apply(
             means: Array[Array[Double]],
             covariances: Array[Array[Array[Double]]],
             weights: Array[Double]): GMM =
    val components =
      (means zip covariances zip weights).map:
        case ((m, c), w) => Component(m, c, w)

    GMM(IArray.unsafeFromArray(components))

  def dilate(gmm: GMM, f: Double): GMM =
    def dilatedComponents = gmm.components.map(c => c.copy(covariance = c.covariance.map(_.map(_ * math.pow(f, 2)))))

    gmm.copy(dilatedComponents)

  def toDistribution(gmm: GMM, random: Random): MixtureMultivariateNormalDistribution =
    import org.apache.commons.math3.distribution._
    import org.apache.commons.math3.util._

    import scala.jdk.CollectionConverters._

    def dist = (gmm.means zip gmm.covariances).map { case (m, c) => new MultivariateNormalDistribution(mgo.tools.apacheRandom(random), m, c) }
    def pairs = (dist zip gmm.weights).map { case (d, w) => new Pair(java.lang.Double.valueOf(w), d) }.toList

    new MixtureMultivariateNormalDistribution(mgo.tools.apacheRandom(random), pairs.asJava)

  case class Component(mean: Array[Double], covariance: Array[Array[Double]], weight: Double)

  def empty: GMM = GMM(Seq.empty)

  extension (gmm: GMM)
    def means: Array[Array[Double]] = gmm.components.map(_.mean).toArray
    def covariances: Array[Array[Array[Double]]] = gmm.components.map(_.covariance).toArray
    def weights: Array[Double] = gmm.components.map(_.weight).toArray
    def size: Int = gmm.components.size
    def isEmpty: Boolean = gmm.components.isEmpty

case class GMM(components: Seq[GMM.Component])


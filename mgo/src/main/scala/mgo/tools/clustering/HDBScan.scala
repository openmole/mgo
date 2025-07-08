package mgo.tools.clustering

import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import org.apache.commons.math3.stat.correlation.Covariance

import scala.jdk.CollectionConverters.*

/**
 * Simplistic implementation of K-Means.
 */
object HDBScan:

  def covariance(x: Array[Array[Double]]) = new Covariance(x).getCovarianceMatrix.getData

  def computeCentroid(points: Array[Array[Double]], weights: Option[Array[Double]]) =
    def average(x: Array[Double], w: Option[Array[Double]]) =
      w match
        case Some(w) => (x zip w).map { case (x, w) => x * w }.sum / w.sum
        case None => x.sum / x.size

    points.transpose.map { coord => average(coord, weights) }

  def clusterize(
    x: Array[Array[Double]],
    minPoints: Int,
    dataWeights: Option[Array[Double]] = None): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
    //val pointSize = x.head.length

    def buildSingleCluster(): (Array[Array[Double]], Array[Array[Array[Double]]], Array[Double]) =
      val centroids = computeCentroid(x, dataWeights)
      val weight = Array(1.0)

      val covariance = HDBScan.covariance(x)
      //        val clusterMatrix = Breeze.arrayToDenseMatrix(x)
      //        val centroidVector = new DenseVector[Double](centroids)
      //        Breeze.matrixToArray(cov(clusterMatrix, centroidVector))

      (Array(centroids), Array(covariance), weight)

    import jsat.clustering.*
    import jsat.clustering.kmeans.*
    import jsat.linear.distancemetrics.*

    val hdbScan = new HDBSCAN
    /*
     Setting the number of neighbors to consider, acts as a smoothing over the density estimate (minPoints) and
     the minimum number of data points needed to form a cluster (minClusterSize) to the same value.
     */
    hdbScan.setMinPoints(minPoints)
    hdbScan.setMinClusterSize(minPoints)

    if x.length <= hdbScan.getMinPoints
    then buildSingleCluster()
    else
      val dataSet =
        val dataPoints =
          dataWeights match
            case Some(dataWeights) =>
              (x zip dataWeights).map: (p, w) =>
                new DataPoint(new jsat.linear.DenseVector(p), w)
            case None =>
              x.map: x =>
                new DataPoint(new jsat.linear.DenseVector(x))

        new SimpleDataSet(dataPoints.toList.asJava)

      val clusters = hdbScan.cluster(dataSet).asScala.map(_.asScala.toArray).toArray

      if !clusters.isEmpty
      then
        val centroids =
          clusters.map: cluster =>
            val points = cluster.map(_.getNumericalValues.arrayCopy())
            val weights = cluster.map(_.getWeight)
            computeCentroid(points, Some(weights))

        val totalWeight = clusters.flatten.map(_.getWeight).sum
        val weights = clusters.map(_.map(_.getWeight).sum / totalWeight)

        val covariances = clusters.map(c => HDBScan.covariance(c.map(_.getNumericalValues.arrayCopy())))

        (centroids, covariances, weights)
      else buildSingleCluster()




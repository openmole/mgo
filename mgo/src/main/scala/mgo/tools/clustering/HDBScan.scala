package mgo.tools.clustering


import jsat.clustering.*
import jsat.clustering.kmeans.*
import jsat.linear.distancemetrics.*
import jsat.SimpleDataSet
import jsat.classifiers.DataPoint
import scala.jdk.CollectionConverters.*
import org.apache.commons.math3.stat.correlation.Covariance

/**
 * Simplistic implementation of K-Means.
 */
object HDBScan:

  def clusterize(x: Array[Array[Double]], minPoints: Int): Array[Array[Array[Double]]] =
    def buildSingleCluster(): Array[Array[Array[Double]]] = Array(x)

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
          x.map: x =>
            new DataPoint(new jsat.linear.DenseVector(x))

        new SimpleDataSet(dataPoints.toList.asJava)

      val clusters = hdbScan.cluster(dataSet).asScala.map(_.asScala.toArray).toArray

      if !clusters.isEmpty
      then
        clusters.map: cluster =>
          cluster.map(_.getNumericalValues.arrayCopy())
      else buildSingleCluster()




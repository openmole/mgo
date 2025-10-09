package mgo.tools.metric

object EskinDistance:
  def eskinDistance(x: Int, y: Int, k: Int): Double =
    if x == y
    then 0.0
    else 2.0 / (k.toDouble * k.toDouble)

  def maxDistance(k: Int) = 2.0 / (k.toDouble * k.toDouble)

  def normalizedEskinDistance(x: Seq[Int], y: Seq[Int], k: Seq[Int]) =
    val total = k.map(maxDistance).sum
    (x lazyZip y lazyZip k).map(eskinDistance).sum / total

  def averageDiversity(values: Vector[IArray[Int]], cardinalities: Vector[Int]) =
    if cardinalities.nonEmpty
    then
      for
        (d1, i1) <- values.zipWithIndex
      yield
        val total =
          values.zipWithIndex.filter(_._2 != i1).map: (d2, _) =>
            EskinDistance.normalizedEskinDistance(d1, d2, cardinalities)
          .sum
        total / (values.size - 1)
    else values.map(_ => 0.0)


// Goodall Distance (variant 3) implementation in Scala 3
object GoodallDistance:

  /** Compute category probabilities for each categorical feature (column). */
  def computeFrequencies(data: Seq[IArray[Int]]): Seq[Map[Int, Double]] =
    val n = data.size.toDouble
    val m = data.head.size
    (0 until m).map: j =>
      val counts = data.map(_(j)).groupBy(identity).view.mapValues(_.size.toDouble / n).toMap
      counts

  /** Compute Goodall 3 similarity for two records given the column frequency maps. */
  def goodall3Similarity(x: Seq[Int], y: Seq[Int], freqs: Seq[Map[Int, Double]]): Double =
    val sims = x.indices.map: j =>
      if x(j) == y(j) then
        val p = freqs(j)(x(j))
        // sum of squared probabilities for categories rarer or equal in frequency
        freqs(j).filter((_, pj) => pj <= p).values.map(pj => pj * pj).sum
      else 0.0

    sims.sum / x.size

  /** Compute Goodall 3 distance = 1 - similarity */
  def goodall3Distance(x: Seq[Int], y: Seq[Int], freqs: Seq[Map[Int, Double]]): Double =
    1.0 - goodall3Similarity(x, y, freqs)

  def averageDiversity(values: Vector[IArray[Int]]) =
    val frequency = computeFrequencies(values)

    for
      (d1, i1) <- values.zipWithIndex
    yield
      val total =
        values.zipWithIndex.filter(_._2 != i1).map: (d2, _) =>
          goodall3Distance(d1, d2, frequency)
        .sum
      total / (values.size - 1)
  
  def knnDiversity(values: Vector[IArray[Int]], n: Int = 4) =
    val frequency = computeFrequencies(values)

    for
      (d1, i1) <- values.zipWithIndex
    yield
      val total =
        values.zipWithIndex.filter(_._2 != i1).map: (d2, _) =>
          goodall3Distance(d1, d2, frequency)
        .sorted
        .take(n)
        .sum
      total / n
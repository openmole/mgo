package mgo.tools.metric

/*
 * Copyright (C) 2025 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import scala.collection.mutable.ListBuffer


/**
 *
 * This is a translation from Java MOEAFramework to Scala
 *
 * Fast hypervolume calculation published by the Walking Fish Group (WFG).  This implementation includes all
 * optimizations discussed in the paper, including: (1) sorting the solutions by an objective, (2) slicing, and
 * (3) an exact method to compute the 2D hypervolume case.
 * <p>
 * This version is not normalized!  See {@link WFGNormalizedHypervolume} for the normalized version.
 * <p>
 * References:
 * <ol>
 * <li>While, Ronald Lyndon et al. "A Fast Way of Calculating Exact Hypervolumes." IEEE Transactions on Evolutionary
 * Computation 16 (2012): 86-95.
 * </ol>
 */
object WFGHypervolume:

  /**
   * Comparator to sort solutions so they are monotonically improving (decreasing) in the last objective.
   * Ties are broken by considering other objectives.
   */
  def objectiveComparator(slice: Int): Ordering[IArray[Double]] = (s1, s2) =>
    import scala.util.boundary

    boundary:
      for
        i <- slice - 1 to 0 by -1
      do
        val flag = java.lang.Double.compare(s1(i), s2(i))
        if flag != 0 then boundary.break(-flag)

      0



  def nomalize(points: IArray[IArray[Double]]): IArray[IArray[Double]] =
    if points.isEmpty
    then points
    else
      val d = points.head.length

      val mins = (0 until d).map(j => points.map(_(j)).min)
      val maxs = (0 until d).map(j => points.map(_(j)).max)

      points.map: f =>
        IArray.from:
          (0 until d).map: j =>
            val denom = (maxs(j) - mins(j)) max mgo.tools.epsilon
            (f(j) - mins(j)) / denom


  def normalizedHypervolumeContribution(points: IArray[IArray[Double]]) =
    import cats.Later
    if points.isEmpty
    then Vector()
    else
      val nPoints = nomalize(points)
      val refPoint = IArray.fill(nPoints.head.length)(1.1)

      val hvAll = Later(hypervolume(nPoints, refPoint))

      nPoints.indices.map: i =>
        Later:
          val reduced = nPoints.patch(i, Nil, 1)
          val hvReduced = hypervolume(reduced, refPoint)
          hvAll.value - hvReduced
      .toVector

//
//  // Usage:
//  val (normalizeSet, mins, maxs) = normalizeObjectives(fitness, population)
//  val normalized = normalizeSet(population)
//  val reference  = Vector.fill(normalized.head.length)(1.05)

  //def contributions(pointList: IArray[IArray[Double]], dimension: Int)

  def hypervolume(pointList: IArray[IArray[Double]], referencePoint: IArray[Double]) =
    if pointList.isEmpty
    then 0.0
    else
      val dimension = pointList.head.length
      wfg(pointList, referencePoint, dimension)

  /**
   * Recursive hypervolume calculation using slices.
   *
   * @param pointList the list of points being considered in the hypervolume calculation, should be a non dominated set of points
   * @param slice     the current slice (dimension) being computed
   * @return the hypervolume of the points
   */
  def wfg(pointList: IArray[IArray[Double]], referencePoint: IArray[Double], dimension: Int): Double =
    if pointList.isEmpty
    then 0.0
    else
      val sortedPointList = pointList.sorted(using WFGHypervolume.objectiveComparator(dimension))

      if dimension == 1
      then Math.abs(sortedPointList(0)(0) - referencePoint(0))
      else
        if dimension == 2
        then
          var volume =
            Math.abs(sortedPointList(0)(0) - referencePoint(0)) *
              Math.abs(sortedPointList(0)(1) - referencePoint(1))

          for
            i <- 1 until sortedPointList.size
          do
            volume +=
              Math.abs(sortedPointList(i)(0) - referencePoint(0)) *
                Math.abs(sortedPointList(i - 1)(1) - sortedPointList(i)(1))

          volume
        else
          var volume = 0.0
          for
            i <- sortedPointList.size - 1 to 0 by -1
          do
            volume += Math.abs(sortedPointList(i)(dimension - 1) - referencePoint(dimension - 1)) *
              exclhv(sortedPointList, referencePoint, i, dimension - 1)

          volume

  /**
   * Returns the inclusive hypervolume for the given point.  Inclusive means it measures the entire volume
   * dominated by this point.
   *
   * @param point the current point
   * @param slice the current slice (dimension) being computed
   * @return the inclusive hypervolume of the solution
   */
  def inclhv(point: IArray[Double], referencePoint: IArray[Double], slice: Int) =
    var volume = 1.0

    for
      i <- 0 until slice
    do volume *= Math.abs(point(i) - referencePoint(i))

    volume

  /**
   * Returns the exclusive hypervolume of the current (contributing) point.  Any volume overlapping with outer points
   * is excluded.
   *
   * @param pointList the list of points being considered in the hypervolume calculation
   * @param k         the index of the current (contributing) point
   * @param slice     the current slice (dimension) being computed
   * @return the exclusive hypervolume of the current solution
   */
  def exclhv(pointList: IArray[IArray[Double]], referencePoint: IArray[Double], k: Int,  slice: Int) =
    inclhv(pointList(k), referencePoint, slice) - wfg(limitset(pointList, k, slice), referencePoint, slice)


  /**
   * Returns a modified list of the points {k+1 .. |pointList|} updated with the larger value taken from the point
   * and the contributing point k.  Additionally, any dominated points in this modified list are removed.
   * <p>
   * Note: This method is equivalent to {@code nds(limitset(...))} in Figure 5 from the referenced paper.
   *
   * @param pointList the list of points being considered in the hypervolume calculation
   * @param k         the index of the current (contributing) point
   * @param slice     the current slice (dimension) being computed
   * @return a new list of points with updated objective values
   */
  def limitset(pointList: IArray[IArray[Double]], k: Int, slice: Int) =
    import mgo.evolution.dominance.nonStrictDominance

    val result = ListBuffer[IArray[Double]]()
    for
      i <- 0 until pointList.size - k - 1
    do
      val slicedSolution =
        IArray.tabulate(slice): j =>
          worse(pointList(k), pointList(k + i + 1), j)

      result.filterInPlace(s => !nonStrictDominance.isDominated(s, slicedSolution))

      if !result.exists(s => nonStrictDominance.isDominated(slicedSolution, s))
      then result += slicedSolution

    IArray.from(result)
  /**
   * Returns the worse objective value between the two points.  Since all objectives are minimized, this returns
   * the larger value.
   *
   * @param point1    the first point
   * @param point2    the second point
   * @param objective the objective to compare
   * @return the worse (larger) objective value
   */
  def worse(point1: IArray[Double], point2: IArray[Double], objective: Int): Double =
    Math.max(point1(objective), point2(objective))


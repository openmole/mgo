/*
 * Copyright (C) 2011 sebastien rey
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
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy
import scala.math._

trait HypervolumeDiversity extends DiversityMetric with ReferencePoint with Dominance {
  
  def diversity(evaluated: IndexedSeq[(Individual[G], Lazy[Int])]) = {

   lazy val fronts = evaluated.map{_._1}.map{ ind => ind.fitness.values}

   lazy val rp = referencePoint(fronts)

   // Lazy method computation of global contribution for all front
    //Class individual by group of rank
    val groupOfFront = evaluated.zipWithIndex.map{
      case ((i, r), index) => (i, r, index)}.groupBy {
      case (i,r,index) => r()}.values

    //Return contribution
    val contributionByPoint = groupOfFront.map{ front =>  computeHypervolume(front, rp)}.toIndexedSeq
    //Merge group of front, and reclass individual by initial index
    val orderingByInitialIndex = contributionByPoint.flatten.sortBy{case (contribution,index) => index}
    //return only the contribution
    orderingByInitialIndex.map{case (contribution,index) => contribution}
  }

  def shadowMap[A, B](xs: IndexedSeq[A])(f: A => B) = {
    val ys = xs map f
    for (i <- ys.indices; (as, bs) = ys splitAt i) yield (as ++ bs.tail , i)
  }

  //Compute for each front
  def computeHypervolume (front:IndexedSeq[(Individual[G],Lazy[Int],Int)],referencePoint:Seq[Double]):IndexedSeq[(Lazy[Double],Int)] = {

   //return an indexedSeq of (IndexedSeq[Double],index)
   val frontValues = front.map{case(ind,r,i) =>  (ind.fitness.values,i)}

   lazy val globalHypervolume = Hypervolume(frontValues.map {e => e._1}, referencePoint, this)

   //compute a new collection with automatic removed incremental of frontValues item by item
    shadowMap(frontValues){case(e,indexShadowed) =>
      (e,indexShadowed)}.map{ case(e,indexShadowed)  =>
      (Lazy(globalHypervolume - Hypervolume(e.map{_._1},referencePoint, this)),frontValues(indexShadowed)._2) }
  }

}

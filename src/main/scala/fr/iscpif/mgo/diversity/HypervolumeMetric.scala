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
import fr.iscpif.mgo.metric.HyperVolume
import scala.math._
trait HypervolumeMetric extends DiversityMetric{ this: GAEvolution with Ranking =>

  //Ok et le pire front est calculé comme d'habitude, meme principe que pour le crowding, en lazy.
  def diversity(evaluated: IndexedSeq[(Individual[G], Lazy[Int])]) = {

   // Transform individual to list of all genomes values in all front
   lazy val fronts = evaluated.map{_._1}.map{ ind => ind.genome.values}

   lazy val referencePoint = fronts.reduce {
      (i1, i2) => (i1 zip i2).map {
        case (i1, i2) => max(i1, i2)
      }
   }

   // Lazy method computation of global contribution for all front
   // Hum qu'est ce que je renvoie ?
   val front = evaluated.groupBy{case (i,r) => r}.values.map{_.unzip._1}.view.map{ front =>
    Lazy(computeHypervolume(front.map {ind => ind.genome.values}, referencePoint))}
  }

  def shadowMap[A, B](xs: IndexedSeq[A])(f: A => B) = {
    val ys = xs map f
    for (i <- ys.indices; (as, bs) = ys splitAt i) yield as ++ bs.tail
  }

  def computeHypervolume (front:IndexedSeq[IndexedSeq[Double]],referencePoint:IndexedSeq[Double]): IndexedSeq[Lazy[Double]] = {

    val globalHypervolume = HyperVolume(front, referencePoint)
    // calculer la contribution de chaque point
    shadowMap(front){e => e}.map{ e => Lazy(globalHypervolume - HyperVolume(e,referencePoint)) }

  }

}

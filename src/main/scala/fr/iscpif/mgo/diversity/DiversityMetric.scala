/*
 * Copyright (C) 2012 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
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

package fr.iscpif.mgo.diversity

import fr.iscpif.mgo._
import tools.Lazy

/**
 * Layer of the cake that compute a diversity metric for a set of individuals
 */
trait DiversityMetric extends G with Ranking {
  
  /**
   * Compute the diversity of the individual
   * 
   * @param individuals a set of individual
   * @return a diversity sequence in the diversity of the individual i at the
   * position i
   */
  def diversity(individuals: IndexedSeq[(Individual[G], Lazy[Int])]): IndexedSeq[Lazy[Double]]
}

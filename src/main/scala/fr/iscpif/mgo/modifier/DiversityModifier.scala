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

package fr.iscpif.mgo.modifier

import fr.iscpif.mgo._
import fr.iscpif.mgo.tools.Lazy
import monocle.Macro._

object DiversityModifier {
  case class Diversity(diversity: Lazy[Double]) {
    override def toString = diversity.toString()
  }

  def toPopulationElements[G, P, F](
    evaluated: Seq[Individual[G, P, F]],
    diversities: Seq[Lazy[Double]]) =
    (evaluated zip diversities) map {
      case (i, d) => PopulationElement(i, Diversity(d))
    }
}

trait DiversityModifier extends Modifier with DiversityMetric with DiversityMF with MG {
  type MF = DiversityModifier.Diversity

  override def diversity = mkLens("diversity")

  override def modify(evaluated: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] =
    DiversityModifier.toPopulationElements[G, P, F](evaluated, diversity(evaluated.map(e => fitness(e))))

}

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
import scalaz.Lens
import fr.iscpif.mgo.tools.Lazy

/**
 * Compute a meta-fitness with a rank an a diversity
 */

object RankDiversityModifier {
  case class RankDiversity(rank: Lazy[Int], diversity: Lazy[Double]) {
    override def toString = rank + " " + diversity
  }

  def toPopulationElements[G, P, F](
    evaluated: Seq[Individual[G, P, F]],
    ranks: Seq[Lazy[Int]],
    distances: Seq[Lazy[Double]]) =
    (evaluated zip ranks zip distances) map {
      case ((i, r), d) =>
        PopulationElement(
          i,
          RankDiversity(
            diversity = d,
            rank = r
          )
        )
    }
}

trait RankDiversityModifier <: Modifier with Ranking with DiversityMetric {

  type F <: MGFitness

  type MF = RankDiversityModifier.RankDiversity

  def diversity: Lens[MF, Lazy[Double]] = Lens.lensu[MF, Lazy[Double]]((c, v) => c.copy(diversity = v), _.diversity)
  def rank: Lens[MF, Lazy[Int]] = Lens.lensu[MF, Lazy[Int]]((c, v) => c.copy(rank = v), _.rank)

  override def modify(evaluated: Seq[Individual[G, P, F]], archive: A): Population[G, P, F, MF] = {
    val f = fitnesses(evaluated, archive)
    val ranks = rank(f)
    val distances = diversity(f, ranks)
    RankDiversityModifier.toPopulationElements[G, P, F](evaluated, ranks, distances)
  }

  def fitnesses(evaluated: Seq[Individual[G, P, F]], archive: A) = evaluated.map(_.fitness.values)
}

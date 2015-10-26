/*
 * Copyright (C) 2012 reuillon
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

package fr.iscpif.mgo

import scalaz._

trait Crossover <: Pop { this: Algorithm =>
  type Crossover = ((G, G) => State[EvolutionState, (G, G)])
}


trait CrossoverDefault <: Crossover { this: Algorithm =>

  def blxCrossover(alpha: Double = 0.5)(implicit values: monocle.Lens[G, GenomeValue[Seq[Double]]])  = new Crossover {
    def apply(g1: G, g2: G) = State { state: EvolutionState =>
          val (newG1, newG2) =
            (values.get(g1).value zip values.get(g2).value).map {
              case (c1, c2) =>
                val cmin = math.min(c1, c2)
                val cmax = math.max(c1, c2)
                val i = cmax - cmin
                def generate = state.random.nextDouble().scale(cmin - alpha * i, cmax + alpha * i)
                (generate, generate)
            }.unzip
          state -> (values.set(GenomeValue(newG1))(g1), values.set(GenomeValue(newG2))(g2))
    }

  }

  //  def apply[G, P, F, A, BreedingContext[_]: Monad](alpha: Double = 0.5)(values: Lens[G, Seq[Double]]) = {
  //    (indivs: Seq[Individual[G, P, F]], population: Population[G, P, F], archive: A, rng: Random) =>
  //      {
  //        val genomes = indivs.map { _.genome }
  //        val (g1, g2) = (genomes(0), genomes(1))
  //        val (newG1, newG2) =
  //          (values.get(g1) zip values.get(g2)).map {
  //            case (c1, c2) =>
  //              val cmin = math.min(c1, c2)
  //              val cmax = math.max(c1, c2)
  //              val i = cmax - cmin
  //              def generate = rng.nextDouble().scale(cmin - alpha * i, cmax + alpha * i)
  //              (generate, generate)
  //          }.unzip
  //        Vector(g1 &|-> values set newG1, g2 &|-> values set newG2).point[BreedingContext]
  //      }
  //  }
}
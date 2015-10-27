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

import scala.util.Random
import tools._
import scalaz._

trait Crossover <: Pop { this: Algorithm =>
  type Crossover = ((G, G) => State[AlgorithmState, (G, G)])
}


trait CrossoverDefault <: Crossover with DynamicOps { this: Algorithm =>

  def blx(alpha: Double = 0.5)(implicit values: monocle.Lens[G, GenomeValue[Seq[Double]]]) = new Crossover {
    def apply(g1: G, g2: G) = State { state: AlgorithmState =>
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

  def dynamicCrossover(genomePart: monocle.Lens[G, Option[Int]], exploration: Double = 0.1)(ops: Crossover*) = (pop: Pop) => new Crossover {
     def apply(g1: G, g2: G) =
       for {
         crossover <- AlgorithmState.random.lifts(dynamicOperator(genomePart, exploration)(ops: _*)(pop))
         res <- crossover(g1, g2)
       } yield res
  }


}
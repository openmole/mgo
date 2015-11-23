/*
 * Copyright (C) 2015 Romain Reuillon
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
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.iscpif.mgo.algorithm

import fr.iscpif.mgo._
import scala.util.Random
import scalaz._
import genome._
import breeding._
import dynamicOps._
import mutation._
import crossover._

object ga {

  case class GAGenome(values: Array[Double] @@ genome.Value, fromOperation: Int = -1)

  implicit val genomeValues =
    monocle.Lens[GAGenome, Seq[Double] @@ genome.Value](g => unwrap(g.values).toSeq)(v => _.copy(values = wrap(v.toArray)))

  implicit def equalsG = Equal.equal[GAGenome]((g1, g2) => g1.genomeValue == g2.genomeValue)

  def intToOption = monocle.Lens[Int, Option[Int]](b => if (b < 0) None else Some(b))(o => _ => o.getOrElse(-1))

  def fromOperation = monocle.macros.Lenser[GAGenome](_.fromOperation) composeLens intToOption

  def randomGenome(size: Int) = State { rng: Random =>
    def genome = GAGenome(wrap(Seq.fill(size)(rng.nextDouble).toArray))
    (rng, genome)
  }

  def newGenomes[P, S](selection: State[AlgorithmState[S], Individual[GAGenome, P]], pop: Population[Individual[GAGenome, P]], operationExploration: Double): State[AlgorithmState[S], Vector[GAGenome]] =
    for {
      op <- random[S] lifts dynamicOperator(pop, fromOperation, operationExploration, operations[S].zipWithIndex)
      ((c, m), i) = op
      s1 <- selection
      s2 <- selection
      res <- breed(c, m)(s1, s2)
    } yield res.map(fromOperation.set(Some(i))).map(g => clamp(g)).toVector

  def operations[S]: Vector[(Crossover[GAGenome, S], Mutation[GAGenome, S])] =
    for {
      c <- crossover[S]
      m <- mutations[S]
    } yield (c, m)

  def mutations[S] = Vector[Mutation[GAGenome, S]](
    bga[GAGenome, S](mutationRate = 1.0 / _, mutationRange = 0.001),
    bga[GAGenome, S](mutationRate = 1.0 / _, mutationRange = 0.01),
    bga[GAGenome, S](mutationRate = 2.0 / _, mutationRange = 0.1),
    bga[GAGenome, S](mutationRate = _ => 0.5, mutationRange = 0.5)
  )

  def crossover[S]: Vector[Crossover[GAGenome, S]] = Vector(
    blx[GAGenome, S](0.1),
    blx[GAGenome, S](0.5),
    blx[GAGenome, S](2.0),
    sbx[GAGenome, S](0.1),
    sbx[GAGenome, S](0.5),
    sbx[GAGenome, S](2.0)
  )

}


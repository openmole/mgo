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

trait GeneticAlgorithm <: Algorithm with MutationFunctions with CrossoverFunctions with BreedingFunctions {

  case class GAGenome(values: Array[Double] @@ Genome.Value, fromOperation: Int = -1)
  type G = GAGenome

  implicit val genomeValues =
    monocle.Lens[G, Seq[Double] @@ Genome.Value](g => unwrap(g.values).toSeq)(v => _.copy(values = wrap(v.toArray)))

  implicit def equalsG = Equal.equal[G]((g1, g2) => g1.genomeValue == g2.genomeValue)

  def randomGenome(size: Int) = State { rng: Random =>
    def genome = GAGenome(wrap(Seq.fill(size)(rng.nextDouble).toArray))
    (rng, genome)
  }

  private def intToOption = monocle.Lens[Int, Option[Int]](b => if(b < 0) None else Some(b))(o => _ => o.getOrElse(-1))

  def fromOperation =
    monocle.macros.Lenser[G](_.fromOperation) composeLens intToOption

  def operationExploration = 0.1

  def newGenomes(selection: State[AlgorithmState, Ind], pop: Pop): State[AlgorithmState, Vector[G]] =
    for {
      op <- random.lifts(dynamicOperator(pop, fromOperation, operationExploration, operations.zipWithIndex))
      ((c, m), i) = op
      s1 <- selection
      s2 <- selection
      res <- breed(c, m)(s1, s2)
    } yield res.map(fromOperation.set(Some(i))).map(clamp).toVector

  def operations =
    for {
      c <- crossover
      m <- mutations
    } yield (c, m)


  def mutations = Vector[Mutation](
    bga(mutationRate = 1.0 / _, mutationRange = 0.001),
    bga(mutationRate = 1.0 / _, mutationRange = 0.01),
    bga(mutationRate = 2.0 / _, mutationRange = 0.1),
    bga(mutationRate = _ => 0.5, mutationRange = 0.5)
  )

  def crossover = Vector[Crossover](
    blx(0.1),
    blx(0.5),
    blx(2.0),
    sbx(0.1),
    sbx(0.5),
    sbx(2.0)
  )

}



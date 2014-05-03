/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.iscpif.mgo.test

import scala.util.Random
import fr.iscpif.mgo.problem.{ GAProblem, Problem }
import fr.iscpif.mgo.modelfamily.ModelFamilyGenome
import fr.iscpif.mgo.fitness.MGFitness

object RastriginVector {

  def gray(n: Int) = n ^ (n >>> 1)

  def toBitSet(n: Int) =
    Iterator.iterate(n)(n => n >> 1).map {
      v => (v & 0x0001) == 1
    }.take(32).toVector

  def bitSet(size: Int) =
    (0 until size).map { gray }.map { toBitSet }

}

trait RastriginVector <: GAProblem with ModelFamilyGenome {

  def models: Int

  lazy val masks: Seq[Seq[Boolean]] = (0 until models).map(_ => RastriginVector.toBitSet(genomeSize))
  lazy val min = Seq(0.0) ++ Seq.fill(genomeSize)(-5.12)
  lazy val max = Seq(masks.size.toDouble) ++ Seq.fill(genomeSize)(5.12)

  type P = Double

  override def express(g: G, rng: Random) = {
    val id = modelId.get(g)

    val vector =
      (masks(id) zip values.get(g)).flatMap {
        case (true, v) => Some(v)
        case _ => None
      }
    Rastrigin.value(vector)
  }

  override def evaluate(phenotype: P, rng: Random): F = MGFitness(phenotype)

}

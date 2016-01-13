/**
 * Created by Romain Reuillon on 13/01/16.
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
 *
 */
package fr.iscpif.mgo.rjmcmc

import fr.iscpif.mgo.contexts.RandomGen
import monocle.macros.Lenses

import scala.util.Random
import scalaz._
import Scalaz._

object sampler {

  type DirectSampler[M[_], C] = M[C]
  type Sampler[M[_], C] = Kleisli[M, C, C]

  case class Transformed[C](
      c1: C,
      modification: C => C,
      pdf0: Double,
      pdf1: Double,
      v0: Double,
      v1: Double,
      jacobian: Double) {
    def c2 = modification(c1)
  }

  type Transform[M[_], C] = Kleisli[M, C, Transformed[C]]

  def rjmcmcSampler[M[_]: Monad: RandomGen, C](
    transform: Transform[M, C]): Sampler[M, C] =
    transform andThen applyTransform[M, C]

  def applyTransform[M[_]: Monad: RandomGen, C] = Kleisli[M, Transformed[C], C] { c =>
    implicitly[RandomGen[M]].random.map {
      rng =>
        import c._
        val acceptance = (pdf0 * v0 * jacobian) / (pdf1 * v1)
        if (rng.nextDouble < acceptance) c2 else c1
    }
  }

  def identityTransform[M[_]: Monad, C]: Transform[M, C] = Kleisli { c =>
    Transformed(c, identity[C], 1.0, 1.0, 1.0, 1.0, 1.0).point[M]
  }

  def move[M[_]: Monad: RandomGen, C](elements: monocle.Lens[C, Vector[Double]]): Transform[M, C] = Kleisli { c =>
    for {
      rng <- implicitly[RandomGen[M]].random
    } yield {
      val modif = elements.modify(_.map(_ + rng.nextGaussian()))
      Transformed(c, modif, 1.0, 1.0, 1.0, 1.0, 1.0)
    }
  }
}

object TestRJMCMC extends App {

  import sampler._
  import fr.iscpif.mgo.contexts.default._

  @Lenses case class Configuration(d: Vector[Double])

  val res =
    unwrap(rjmcmcSampler[EvolutionState[Unit, ?], Configuration](
      move[EvolutionState[Unit, ?], Configuration](Configuration.d)).
      run(Configuration(Vector(0.0, 5.0))), EvolutionData[Unit](s = ()))

  println(res)
}


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

package fr.iscpif.mgo.mutation

import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import tools.Math._
import tools.Random._
import java.util.Random

trait DynamicGaussianMutation extends Mutation  { 
  self: SigmaGAEvolution =>
  
  //http://c-faq.com/lib/gaussian.html
  //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
  //http://www.taygeta.com/random/gaussian.html
  override def mutate (genome : G) (implicit aprng : Random, factory: Factory[G]) : G = {
    val newValues = (genome.values zip genome.sigma) map {
      case (v, s) => clamp (aprng.nextGaussian * s + v, 0, 1)
    }
    factory(genome.updatedValues(newValues))
  }
}

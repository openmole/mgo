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

import java.util.Random
import fr.iscpif.mgo._
import fr.iscpif.mgo.ga._
import fr.iscpif.mgo.tools.Random._

trait RandomMutation extends Mutation  { 
  self: GAEvolution =>
    
  def mutationRate = 0.5

  override def mutate(genome: G)(implicit aprng: Random, factory: Factory[G]): G = {
    val randomGenome = factory.random
    val valMutationZipped = genome.content.zip(randomGenome.content)   
    val newValues = valMutationZipped map { 
      case(v,vrg) => 
        if (aprng.nextDouble < mutationRate) vrg else v 
    }
    
    factory(newValues)
  }
  
}

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
import fr.iscpif.mgo.tools.Random._

trait RandomValuesMutation extends Mutation with GAG with GenomeFactory {
  
  def mutationRate: Double = 0.5
  
  override def mutate(genome: G)(implicit aprng: Random): G = {
   
    /* FIXME Faire en sorte d'utiliser plutot un genome généré par buildRandomGenome, 
     * plutot qu'une valeur tiré au hasard avec aprng... */
    val newValues = genome.values map { v => 
      if (aprng.nextDouble < mutationRate) aprng.nextDouble
      else v 
    }
    genomeFactory(genome.updatedValues(newValues))
  }
  
}

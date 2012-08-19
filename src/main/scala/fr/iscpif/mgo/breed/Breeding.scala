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

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import java.util.Random

trait Breeding extends Lambda with G with MF with Selection with CrossOver with Mutation with GenomeFactory {
  
  def breed(archive: Population[G, MF])(implicit aprng: Random): IndexedSeq[G] = breed(archive, lambda)
  
  def breed(archive: Population[G, MF], size: Int)(implicit aprng: Random): IndexedSeq[G] =  
    Iterator.continually {
      if(archive.isEmpty) IndexedSeq(genomeFactory.random)
      else crossover(selection(archive).genome, selection(archive).genome).map { mutate(_) }
    }.flatten.take(size).toIndexedSeq
    
}

/*
 *  Copyright (C) 2010 Romain Reuillon <romain.reuillon at openmole.org>
 * 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package org.openmole.tools.mgo.tools

import java.util.{Random => JRandom}
import collection.mutable.ArrayBuffer

object Random {
  
  implicit def indexedSeq2IndexedSeqDecorator[T](elts: IndexedSeq[T]) = new IndexedSeqDecorator(elts)
  
  class IndexedSeqDecorator[T](elts: IndexedSeq[T]) {
    def random(implicit prng: JRandom) = elts(prng.nextInt(elts.size))
    def shuffle(implicit prng: JRandom) = {
      val buf = new ArrayBuffer[T] ++= elts
	       
      def swap(i1: Int, i2: Int) {
        val tmp = buf(i1)
        buf(i1) = buf(i2)
        buf(i2) = tmp
      }
	   
      for (n <- buf.length to 2 by -1) {
        val k = prng.nextInt(n)
        swap(n - 1, k)
      }
	   
      buf.toIndexedSeq      
    }
  }
  
  /*def rndmChoice(set: T*)(implicit rng: Random): T = {
   set(rng.nextInt(set.length))
   }*/

  def rndmChoice[T](t1: T, t2: T)(implicit rng: JRandom): T = {
    if(rng.nextDouble <  0.5) t1 else t2
  }
  
  
}

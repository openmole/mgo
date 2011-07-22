/*
 * Copyright (C) 2011 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.asrea
/*
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.openmole.tools.mgo.asrea._
import org.openmole.tools.mgo.evolution._
import org.openmole.tools.mgo.tools.Random._
import java.io.File
import org.openmole.tools.mgo.model._
import scala.collection.mutable.ListBuffer
import java.util.Random

@RunWith(classOf[JUnitRunner])
class ASREASpec extends FlatSpec with ShouldMatchers {

  "ASREA" should "converge to good solutions" in {
    class TestGenome(val v1: Double, val v2: Double)
    
    val mutation = new GenomeOperation[TestGenome] {
      def operate(population: IndexedSeq[TestGenome])(implicit aprng: Random) = population.random
    }
    
    val evolutionEngine = new EvolutionEngine(mutation)
    
    def fitness(tg: TestGenome) = MultiGoal.buildDouble(tg.v1, tg.v2)

    val ag = new ASREA(evolutionEngine, fitness)
    
    val rng = new Random
    val pop = (0 until 20).map{i => new TestGenome(rng.nextDouble, rng.nextDouble)}.map{g => new Individual(g, fitness(g))}
    val archive = (0 until 20).map{i => new TestGenome(rng.nextDouble, rng.nextDouble)}.map{g => new Individual(g, fitness(g))}
    
    ag.evolve(pop, archive, 20)(rng)
    true should equal(true)
  }
  
}
*/

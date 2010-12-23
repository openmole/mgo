/*
 * Copyright (C) 2010 reuillon
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

package org.openmole.tools.mgo.paretoquick

import org.junit._
import Assert._
import scala.collection.mutable.ArrayBuffer

class ParetoQuickTest {

  @Before
  def setUp: Unit = {
  }

  @After
  def tearDown: Unit = {
  }

  @Test
  def testPareto = {
    var vectMultiGoalDouble = new ArrayBuffer[MultiGoalDouble](14)
    // vectMultiGoalDouble+=new MultiGoalDouble( -1, 2., 44));
    vectMultiGoalDouble += new MultiGoalDouble(-1, 2., 44)
    vectMultiGoalDouble += new MultiGoalDouble(2, 3.4, 4)
    vectMultiGoalDouble += new MultiGoalDouble(1, 7., 9)
    vectMultiGoalDouble += new MultiGoalDouble(0, 2., 0)
    vectMultiGoalDouble += new MultiGoalDouble(3, 0., 9)
    vectMultiGoalDouble += new MultiGoalDouble(4, 44., 0)
    vectMultiGoalDouble += new MultiGoalDouble(1, 7., 9)
    vectMultiGoalDouble += new MultiGoalDouble(3, 2.8, 1)
    vectMultiGoalDouble += new MultiGoalDouble(3, 2., 9)
    vectMultiGoalDouble += new MultiGoalDouble(90, 3.4, 4)
    vectMultiGoalDouble += new MultiGoalDouble(-2, 7., 9)
    vectMultiGoalDouble += new MultiGoalDouble(1, 2., 44)
    vectMultiGoalDouble += new MultiGoalDouble(3, 0., 9)

    vectMultiGoalDouble.foreach{println _}
    
    println("----------------")
    
    ParetoQuick.pareto[Double, MultiGoalDouble](vectMultiGoalDouble, 3).foreach{
      println _
    }

    assertEquals(8, ParetoQuick.pareto[Double, MultiGoalDouble](vectMultiGoalDouble, 3).size);

    vectMultiGoalDouble = new ArrayBuffer[MultiGoalDouble](40)
    vectMultiGoalDouble += new MultiGoalDouble(0.96, 6, 4, 0.3)
    vectMultiGoalDouble += new MultiGoalDouble(0.84000003, 8, 4, 44)
    vectMultiGoalDouble += new MultiGoalDouble(0.96, 8, 3, -1)
    vectMultiGoalDouble += new MultiGoalDouble(0.84000003, 9, 3, 2.0)
    vectMultiGoalDouble += new MultiGoalDouble(0.96, 9, 3, 2.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 10, 3, 10)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 11, 2, 1)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 11, 3, 3)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 11, 4, 11)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 12, 2, 110)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 12, 3, 2)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 13, 3, 7)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 14, 3, 1)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 21, 3, 0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 22, 3, 0.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.75, 22, 4, 3)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 22, 2, 2.2)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 22, 3, 2.6)
    vectMultiGoalDouble+=new MultiGoalDouble(0.96, 22, 3, 2.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.8, 25, 4, 5.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.96, 6, 3, 6.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 8, 2, 9.0)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 8, 3, 1.4)
    vectMultiGoalDouble+=new MultiGoalDouble(0.84000003, 8, 4, 3.3)
    vectMultiGoalDouble+=new MultiGoalDouble(0.94000003, 9, 5, 4.3)
    println   
    println("----------------")
    
    vectMultiGoalDouble.foreach{println _}
    
    println("----------------")
    
    ParetoQuick.pareto[Double, MultiGoalDouble](vectMultiGoalDouble, 4).foreach{
      println _
    }

    assertEquals(21, ParetoQuick.pareto[Double, MultiGoalDouble](vectMultiGoalDouble, 4).size)
    
  }

}

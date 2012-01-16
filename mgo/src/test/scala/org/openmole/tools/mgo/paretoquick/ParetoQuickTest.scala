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
import scala.collection.mutable.ListBuffer

class ParetoQuickTest {

//  @Before
//  def setUp: Unit = {
//  }
//
//  @After
//  def tearDown: Unit = {
//  }
//
//  @Test
//  def testPareto = {
//    
////    var vectMultiGoalDouble = new ArrayBuffer[MultiGoalDouble](30)
////    
////    vectMultiGoalDouble += MultiGoalDouble(0.33896939481385335,0.24433782754404842,0.38991673785639214,0.20962350549767628)
////    vectMultiGoalDouble += MultiGoalDouble(0.4576772754676125,0.3199079745213034,0.2871669856099186,0.5426859249819243)
////    vectMultiGoalDouble += MultiGoalDouble(0.6553243580733121,0.8165843085792832,0.6227208711662456,0.09933278669669288)
////    vectMultiGoalDouble += MultiGoalDouble(0.4899715207451588,0.3012320248315118,0.9499121588495897,0.031269095879963404)
////    vectMultiGoalDouble += MultiGoalDouble(0.7877817732772407,0.36998950696255484,0.23165143024106816,0.6434017180909355)
////    vectMultiGoalDouble += MultiGoalDouble(0.8981798327967756,0.3362256885698637,0.4341750064317288,0.42199187704551067)
////    vectMultiGoalDouble += MultiGoalDouble(0.9077531099997481,0.9637634758564196,0.7550872937581689,0.45513501692050373)
////    vectMultiGoalDouble += MultiGoalDouble(0.40642997803108794,0.019813956462461646,0.38050265055383425,0.9033132077831496)
////    vectMultiGoalDouble += MultiGoalDouble(0.6973236968353713,0.1964800164875824,0.4795691990479939,0.09280480080369113)
////    vectMultiGoalDouble += MultiGoalDouble(0.9207313862306417,0.25535188362049444,0.7393835440878486,0.751024441475396)
////    vectMultiGoalDouble += MultiGoalDouble(0.42312893309942723,0.0866317687220256,0.11928213864958359,0.8804065213084332)
////    vectMultiGoalDouble += MultiGoalDouble(0.8436047415520597,0.7423879709243786,0.6607213249845156,0.7762330405971555)
////    vectMultiGoalDouble += MultiGoalDouble(0.8130262250760142,0.40295359705442513,0.12743480358879888,0.7338471223316739)
////    vectMultiGoalDouble += MultiGoalDouble(0.8653350572095938,0.6661489066645695,0.9702838886464952,0.0619815599834973)
////    vectMultiGoalDouble += MultiGoalDouble(0.14571832620396408,0.5109200040218799,0.6121922342633904,0.12147142072361061)
////    vectMultiGoalDouble += MultiGoalDouble(0.7670045483943722,0.780151424663455,0.3307836994559187,0.24240825292647894)
////    vectMultiGoalDouble += MultiGoalDouble(0.2134848222987792,0.051823200846941586,0.07981797765471943,0.5378332765631916)
////    vectMultiGoalDouble += MultiGoalDouble(0.020461502497634343,0.3233481564151235,0.4352458283033589,0.8283551764660418)
////    vectMultiGoalDouble += MultiGoalDouble(0.06817749335491907,0.020838170816999013,0.47768188939846246,0.11961754307124628)
////    vectMultiGoalDouble += MultiGoalDouble(0.22038157294896288,0.46321327072032215,0.8303196553475118,0.33333051043032424)
////    vectMultiGoalDouble += MultiGoalDouble(0.7106364047851802,0.060480498388234216,0.25978996139049715,0.2826191176984615)
////    vectMultiGoalDouble += MultiGoalDouble(0.6335774200663243,0.20617429406565912,0.21719080680894964,0.20373113795376585)
////    vectMultiGoalDouble += MultiGoalDouble(0.13368240748746985,0.035863783808946725,0.44749246992249114,0.5086854941565166)
////    vectMultiGoalDouble += MultiGoalDouble(0.4007132369876325,0.9667964789190702,0.3277494959952417,0.8072315538278362)
////    vectMultiGoalDouble += MultiGoalDouble(0.3788426239242356,0.20816242370104032,0.9080263407356546,0.6615288612581114)
////    vectMultiGoalDouble += MultiGoalDouble(0.17558037465329446,0.237125763013316,0.7236712338637739,0.9832457072063098)
////    vectMultiGoalDouble += MultiGoalDouble(0.5169652329042006,0.572686376228794,0.3235484804725941,0.4603140663778088)
////    vectMultiGoalDouble += MultiGoalDouble(0.2370900749182281,0.047596664437790204,0.11606660831078486,0.9093677183600467)
////    vectMultiGoalDouble += MultiGoalDouble(0.7128793479407054,0.1779738775582277,0.573315978366309,0.989928765070911)
////    vectMultiGoalDouble += MultiGoalDouble(0.3591398759400991,0.16091153559001226,0.053356075141210324,0.28769793444217084)
////    
////    
////    ParetoQuick.pareto(vectMultiGoalDouble, 4).foreach{
////      println _
////    }
//    
//    
////
////    println("---------------")
////    
////    vectMultiGoalDouble = new ArrayBuffer[MultiGoalDouble](14)
////    // vectMultiGoalDouble+=new MultiGoalDouble( -1, 2., 44));
////    vectMultiGoalDouble += MultiGoalDouble(-1, 2., 44)
////    vectMultiGoalDouble += MultiGoalDouble(2, 3.4, 4)
////    vectMultiGoalDouble += MultiGoalDouble(1, 7., 9)
////    vectMultiGoalDouble += MultiGoalDouble(0, 2., 0)
////    vectMultiGoalDouble += MultiGoalDouble(3, 0., 9)
////    vectMultiGoalDouble += MultiGoalDouble(4, 44., 0)
////    vectMultiGoalDouble += MultiGoalDouble(1, 7., 9)
////    vectMultiGoalDouble += MultiGoalDouble(3, 2.8, 1)
////    vectMultiGoalDouble += MultiGoalDouble(3, 2., 9)
////    vectMultiGoalDouble += MultiGoalDouble(90, 3.4, 4)
////    vectMultiGoalDouble += MultiGoalDouble(-2, 7., 9)
////    vectMultiGoalDouble += MultiGoalDouble(1, 2., 44)
////    vectMultiGoalDouble += MultiGoalDouble(3, 0., 9)
////
////    vectMultiGoalDouble.foreach{println _}
////    var nbPareto = 0
////    vectMultiGoalDouble.foreach{ x => {
////        vectMultiGoalDouble.find( isDominated(x, _)) match {
////          case None => nbPareto += 1
////          case Some(y) =>
////        }
////      }
////    }
////    println("---------------- expected " + nbPareto)
////    
////    ParetoQuick.pareto(vectMultiGoalDouble, 3).foreach{
////      println _
////    }
////
////    assertEquals(8, ParetoQuick.pareto(vectMultiGoalDouble, 3).size)
////
////    vectMultiGoalDouble = new ArrayBuffer[MultiGoalDouble](40)
////    vectMultiGoalDouble += MultiGoalDouble(0.96, 6, 4, 0.3)
////    vectMultiGoalDouble += MultiGoalDouble(0.84000003, 8, 4, 44)
////    vectMultiGoalDouble += MultiGoalDouble(0.96, 8, 3, -1)
////    vectMultiGoalDouble += MultiGoalDouble(0.84000003, 9, 3, 2.0)
////    vectMultiGoalDouble += MultiGoalDouble(0.96, 9, 3, 2.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 10, 3, 10)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 11, 2, 1)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 11, 3, 3)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 11, 4, 11)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 12, 2, 110)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 12, 3, 2)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 13, 3, 7)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 14, 3, 1)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 21, 3, 0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 22, 3, 0.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.75, 22, 4, 3)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 22, 2, 2.2)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 22, 3, 2.6)
////    vectMultiGoalDouble+= MultiGoalDouble(0.96, 22, 3, 2.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.8, 25, 4, 5.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.96, 6, 3, 6.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 8, 2, 9.0)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 8, 3, 1.4)
////    vectMultiGoalDouble+= MultiGoalDouble(0.84000003, 8, 4, 3.3)
////    vectMultiGoalDouble+= MultiGoalDouble(0.94000003, 9, 5, 4.3)
////   
////    println("----------------")
////    
////    vectMultiGoalDouble.foreach{println _}
////    nbPareto = 0
////    vectMultiGoalDouble.foreach{ x => {
////        vectMultiGoalDouble.find( isDominated(x, _)) match {
////          case None => nbPareto += 1
////          case Some(y) =>
////        }
////      }
////    }
////    
////    
////    println("---------------- expected " + nbPareto)
////    
////    ParetoQuick.pareto(vectMultiGoalDouble, 4).foreach{
////      println _
////    }
////
////    assertEquals(21, ParetoQuick.pareto(vectMultiGoalDouble, 4).size)
////    
//    val vectMultiGoal = new ArrayBuffer[MultiGoal](30)
//    
//    vectMultiGoal += MultiGoal.buildInt(1, 0, 3, 4)
//vectMultiGoal += MultiGoal.buildInt(3, 2, 9, 6)
//vectMultiGoal += MultiGoal.buildInt(2, 0, 4, 1)
//vectMultiGoal += MultiGoal.buildInt(4, 0, 3, 9)
//vectMultiGoal += MultiGoal.buildInt(4, 8, 7, 1)
//vectMultiGoal += MultiGoal.buildInt(5, 0, 4, 1)
//vectMultiGoal += MultiGoal.buildInt(3, 2, 4, 0)
//vectMultiGoal += MultiGoal.buildInt(4, 4, 5, 4)
//vectMultiGoal += MultiGoal.buildInt(8, 0, 6, 9)
//vectMultiGoal += MultiGoal.buildInt(3, 0, 8, 2)
//vectMultiGoal += MultiGoal.buildInt(1, 9, 4, 6)
//vectMultiGoal += MultiGoal.buildInt(4, 5, 1, 9)
//vectMultiGoal += MultiGoal.buildInt(2, 7, 5, 4)
//vectMultiGoal += MultiGoal.buildInt(7, 6, 7, 5)
//vectMultiGoal += MultiGoal.buildInt(6, 0, 4, 7)
//vectMultiGoal += MultiGoal.buildInt(2, 5, 4, 2)
//vectMultiGoal += MultiGoal.buildInt(3, 7, 8, 2)
//vectMultiGoal += MultiGoal.buildInt(5, 1, 4, 4)
//vectMultiGoal += MultiGoal.buildInt(7, 5, 4, 2)
//vectMultiGoal += MultiGoal.buildInt(6, 0, 8, 8)
//vectMultiGoal += MultiGoal.buildInt(8, 7, 8, 1)
//vectMultiGoal += MultiGoal.buildInt(1, 1, 4, 2)
//vectMultiGoal += MultiGoal.buildInt(5, 2, 3, 3)
//vectMultiGoal += MultiGoal.buildInt(0, 8, 3, 8)
//vectMultiGoal += MultiGoal.buildInt(2, 5, 8, 2)
//vectMultiGoal += MultiGoal.buildInt(6, 7, 2, 8)
//vectMultiGoal += MultiGoal.buildInt(6, 9, 9, 3)
//vectMultiGoal += MultiGoal.buildInt(5, 6, 0, 7)
//vectMultiGoal += MultiGoal.buildInt(0, 9, 1, 0)
//vectMultiGoal += MultiGoal.buildInt(1, 0, 6, 9)
//
////  
////    println("----------------")
////    
//    val pareto = new ListBuffer[MultiGoal]
//    vectMultiGoal.foreach{ x => {
//        vectMultiGoal.find( isDominated(x, _)) match {
//          case None => pareto += x
//          case Some(y) =>
//        }
//      }
//    }
//    
//    val paretoQ = ParetoQuick.pareto(vectMultiGoal, 4)
//    println("---------------- expected " + pareto.size + " found " + paretoQ.size)
//    vectMultiGoal.foreach{println _}
//    
//    
//    println("-------------------------pareto-----------------------------")
//    pareto.foreach{println _}
//    
//    println("-------------------------paretoQ-----------------------------")
//    paretoQ.foreach{println _}
//    
//    //assertEquals(pareto.size, paretoQ.size)
//    
//    val paretoRandom = new ListBuffer[MultiGoal]
//    val vectMultiGoalRandom = MultiGoal.buildInt(4, 50,1000)
//    val paretoNaiveRandom = new ListBuffer[MultiGoal]
//    vectMultiGoalRandom.foreach{ x => {
//        vectMultiGoalRandom.find( isDominated(x, _)) match {
//          case None => paretoNaiveRandom += x
//          case Some(y) =>
//        }
//      }
//    }
//    
//    val paretoQRandom = ParetoQuick.pareto(vectMultiGoalRandom, 4)
//    //paretoQRandom.foreach{println _}
//    
//    println("pareto naive " + paretoNaiveRandom.size + " quick " + paretoQRandom.size)
//  }
//  
//  @Test
//  def testSpeed: Unit = {
////    val set = MultiGoalDouble(5, 10000)
////    
////    val time = System.currentTimeMillis
////    val pareto = ParetoQuick.pareto(set, 5)
////    println("Time " + (System.currentTimeMillis - time))
////    println("Size " + pareto.size)
//  }

}

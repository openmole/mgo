/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.CrossOver
import org.openmole.tools.mgo.Individual
import org.openmole.tools.mgo.ga.operators.AverageMutation
import org.openmole.tools.mgo.ga.operators.RandomWrappedValuesCrossOver
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
/** 
 * An example finding a word using genetic algorithm
 */
object Test extends App {
  implicit val aprng = new Random
  
  val wordToFind = "helloworld"
  
  val wordSize = wordToFind.size
  
  class MyGenome (val word : String) extends GAGenome {
    val values = word map (_.hashCode.toDouble)
    override def toString = new String (values map (_.toByte) toArray)
  }
  
  class MyGenomeFactory extends GAGenomeFactory [MyGenome] {
    def buildRandomGenome (implicit aprng: java.util.Random) = {
      val a : Array [Byte] = Array.fill (wordSize) ((97 + aprng.nextInt (26)).toByte)
      new MyGenome (new String (a))
    }
    def buildGenome (v : IndexedSeq[Double]) =
      new MyGenome (new String (v map (_.toByte) toArray))
  }
  
  implicit val factory = new MyGenomeFactory
  
  class MyCrossOver (implicit val factory : MyGenomeFactory) extends CrossOver [MyGenome, MyGenomeFactory] {
    def operate (genomes: IndexedSeq[MyGenome]) (implicit aprng : Random) = {
      val i = aprng.nextInt (genomes(0).values.size)
      val (parent1, parent2) = (genomes.random, genomes.random)
      factory.buildGenome (parent1.values.take (i) ++ parent2.values.drop (i))
    }  
  }
  
  class MyCrossOver2 (implicit val factory : MyGenomeFactory) extends CrossOver [MyGenome, MyGenomeFactory] {
    def operate (genomes: IndexedSeq[MyGenome]) (implicit aprng : Random) = {
      val (parent1, parent2) = (genomes.random, genomes.random)
      val newValues = IndexedSeq.tabulate (genomes(0).values.size) {i =>
        if (aprng.nextBoolean) parent1.values (i)
        else parent2.values (i)
      }
      factory.buildGenome (newValues)
    }
  }
  
  val operators = 
    List (
      new AverageMutation [MyGenome, MyGenomeFactory],
      new MyCrossOver,
      new MyCrossOver2
    )
  
  def fitness (g : MyGenome) : Double =
    (wordToFind map (_.hashCode) zip g.values) map {case (a, b) => 
        scala.math.abs(a - b)} sum

  def generateGenome (pop : IndexedSeq [Individual [MyGenome]]) =
    operators (aprng.nextInt (operators.size)) operate (pop map (_.genome))

  def launch (oldPop : IndexedSeq [Individual [MyGenome]]) : Unit = {
    val genomes = IndexedSeq.fill (oldPop.size) (generateGenome (oldPop))
    val newPop  = genomes map (g => new Individual (g, fitness (g)))
    val bestPop = (oldPop ++ newPop) sortBy (_.fitness)
    println ("Best genome : " + bestPop (0))
    launch (bestPop)
  }
  
  val firstPop = Array.fill (128) {
    val g = factory.buildRandomGenome
    new Individual (g, fitness (g))
  }
  launch (firstPop)
}

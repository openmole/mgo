/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

object Population {
  
  def empty[G, I]: Population[G, I] = IndexedSeq.empty
  //def apply(evaluated: IndexedSeq[(G, Fitness)], individuals: IndexedSeq[I]) = 
}

object PopulationElement {
  def apply[G, I](genome: G, fitness: Fitness, individual: I) = {
    val (g, f, i) = (genome, fitness, individual)
    new PopulationElement[G, I] {
      def genome = g
      def fitness = f
      def individual = i
    }
  }
  
  def apply[G, I <: Individual[G]](i: I) = 
    new PopulationElement[G, I] {
      lazy val genome = i.genome
      lazy val fitness = i.fitness
      lazy val individual = i
    }
}

trait Population[+G, +I]  {
   
  def content: IndexedSeq[PopulationElement[G, I]]
  
  def evaluated: IndexedSeq[(G, Fitness)] = 
    content map {
      e => e.genome -> e.fitness
    }
  
  def individuals: IndexedSeq[I] =
    content map {
      e => e.individual
    }
  
  def evaluatedToIndividuals = evaluated map {case(g, f) => Individual(g, f)}
  
  override def toString = content.toString
  
}

trait PopulationElement[+G, +I] {
  def genome: G
  def fitness: Fitness
  def individual: I
  
  override def equals(o: Any) = (genome, fitness, individual).equals(o)
  override def hashCode = (genome, fitness, individual).hashCode
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", individual = " + individual + ")"
}
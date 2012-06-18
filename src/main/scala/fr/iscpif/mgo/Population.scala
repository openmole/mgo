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
  def apply[G, MF](genome: G, fitness: Fitness, metaFitness: MF) = {
    val (g, f, mf) = (genome, fitness, metaFitness)
    new PopulationElement[G, MF] {
      def genome = g
      def fitness = f
      def metaFitness = mf
    }
  }
  
  def apply[G, MF](i: Individual[G], mf: MF) = 
    new PopulationElement[G, MF] {
      lazy val genome = i.genome
      lazy val fitness = i.fitness
      lazy val metaFitness = mf
    }
}

trait Population[+G, +MF]  {
   
  def content: IndexedSeq[PopulationElement[G, MF]]
  
  def individuals: IndexedSeq[Individual[G]] = content map { _.toIndividual }
  
  override def toString = content.toString
  
}

trait PopulationElement[+G, +MF] {
  def genome: G
  def fitness: Fitness
  def metaFitness: MF
  
  def toIndividual = Individual(genome, fitness)
  
  //override def equals(o: Any) = (genome, fitness, individual).equals(o)
  //override def hashCode = (genome, fitness, individual).hashCode
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", metaFitness = " + metaFitness + ")"
}
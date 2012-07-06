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
  def apply[G, MF](i: Individual[G], mf: MF) = 
    new PopulationElement[G, MF](i.genome, i.fitness, mf)
}

trait Population[+G, +MF]  {

  def content: IndexedSeq[PopulationElement[G, MF]]
  def individuals: IndexedSeq[Individual[G]] = content map { _.toIndividual }
  
  override def toString = content.toString  
}

case class PopulationElement[+G, +MF](val genome: G, val fitness: Fitness, val metaFitness: MF) {
  def toIndividual = Individual(genome, fitness)
  override def toString = "(genome = " + genome + ", fitness = " + fitness + ", metaFitness = " + metaFitness + ")"
}
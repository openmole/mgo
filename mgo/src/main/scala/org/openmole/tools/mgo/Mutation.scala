/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

//TODO : creer un decorateur qui permet d'automatiquement encapsulé un genome simple dans un indexedSeq pour les besoins de genericité entre
//crossover(retour de 2 genomes) et mutation (retour de 1 genome)'
import java.util.Random

trait Mutation [G, F] extends Operator [G, F] {
  def mutate(genomes: IndexedSeq[G], factory: F) (implicit aprng : Random): G
  def apply(genomes: IndexedSeq[G], factory: F) (implicit aprng : Random) = IndexedSeq(mutate(genomes, factory))
}

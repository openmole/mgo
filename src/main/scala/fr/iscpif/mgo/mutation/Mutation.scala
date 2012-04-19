/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.mutation

//TODO : creer un decorateur qui permet d'automatiquement encapsulé un genome simple dans un indexedSeq pour les besoins de genericité entre
//crossover(retour de 2 genomes) et mutation (retour de 1 genome)'
import fr.iscpif.mgo._
import java.util.Random

trait Mutation { this: Evolution =>
  def mutate(genomes: G, factory: F) (implicit aprng : Random): G
}

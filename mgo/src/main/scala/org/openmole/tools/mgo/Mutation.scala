/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

//TODO : creer un decorateur qui permet d'automatiquement encapsulé un genome simple dans un indexedSeq pour les besoins de genericité entre
//crossover(retour de 2 genomes) et mutation (retour de 1 genome)'
abstract class Mutation [G <: AbstractGenome, F <: GenomeFactory [G]]  
  extends Operator [G, F,G] {
}
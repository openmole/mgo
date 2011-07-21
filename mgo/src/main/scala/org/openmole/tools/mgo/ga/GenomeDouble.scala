/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

class GenomeDouble(var chromosomes:Map[String,Double]) {

  def this () = this(chromosomes = Map.empty[String,Double])
  def getChromosomes() = chromosomes
  def apply(key: String): Double = {chromosomes.apply(key)}
  def update(key: String, v: Double) = {chromosomes = chromosomes.updated(key, v)}
  def size: Int = {chromosomes.size}
  def foreach(d: ((String,Double)) => Unit) : Unit = {
    chromosomes.foreach(x => d (x))
  }

}
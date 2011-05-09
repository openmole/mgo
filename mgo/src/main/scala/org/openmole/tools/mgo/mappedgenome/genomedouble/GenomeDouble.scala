/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

class GenomeDouble(chromosomes:Map[String,Double]) {

  def this () = this(chromosomes = Map.empty[String,Double])
  
  def apply(key: String): Double = {chromosomes.apply(key)}
  def update(key: String, v: Double) = {chromosomes.updated(key, v)}
  def size: Int = {chromosomes.size}
  def foreach(d: ((String,Double)) => Unit) : Unit = {
    chromosomes.foreach(x => d (x))
  }
 // def foreach = chromosomes.foreach((String,Double) => Unit) 
  
}

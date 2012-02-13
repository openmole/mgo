/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga.operators.crossover

import org.openmole.tools.mgo._
import ga._
import java.util.Random
import org.openmole.tools.mgo.tools.Random._
import scala.math._
import org.openmole.tools.mgo.tools.Math._

/**
 * SBX RGA operator with Bounded Variable modification, see APPENDIX A p30 into :
 * 
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.33.7291&rep=rep1&type=pdf
 * 
 * @INPROCEEDINGS{Deb98anefficient,
 *   author = {Kalyanmoy Deb},
 *   title = {An Efficient Constraint Handling Method for Genetic Algorithms},
 *   booktitle = {Computer Methods in Applied Mechanics and Engineering},
 *   year = {1998},
 *   pages = {311--338}
 * }
 * 
 * Notes : Deb implementation differs from NSGA2 he proposed on this site : 
 * http://www.iitk.ac.in/kangal/codes.shtml
 *  
 */

class SBXBoundedCrossover[G <: GAGenome, F <: GAGenomeFactory[G]](
  distributionIndex: Double) extends AbstractSBXBoundedCrossover [G, F] {
    
  def crossOver (genomes : IndexedSeq [G], factory: F) (implicit aprng : Random) = 
    crossOver(genomes.random, genomes.random, distributionIndex, factory)
 
}


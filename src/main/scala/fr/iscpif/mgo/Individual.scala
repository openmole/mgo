/*
 *  Copyright (C) 2010 reuillon
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.iscpif.mgo


import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.GAGenome
import fr.iscpif.mgo.tools.Scaling._

object Individual {
  
  import java.io.File

  
  implicit def indexedSeq2IndexedSeqDecorator[G <:GAGenome,F <: GAFitness](individuals:IndexedSeq[Individual[G,F]])= new {
    
    def toMatrix:Array[Array[Double]] = {
      individuals.map{ _.fitness.values.toArray }.toArray 
    }
    
    //Version scale, pas generique :(
    def arrayWithGenome(max:Int,min:Int):Array[Array[Double]]= {
      val nbObjective = individuals.head.fitness.values.size
      var matrix:Array[Array[Double]] = { 
        individuals.map{
          i =>   ((0 until nbObjective).map{ o => i.fitness.values(o)} ++ i.genome.values.map{_.scale(min,max)}).toArray
        }.toArray
      }
      matrix
    }
    
    def toCsv(path:File,arrayOfValues:Array[Array[Double]]) = {
      import java.io._
      import fr.iscpif.mgo.tools.FileUtils._  
      val data = arrayOfValues.map{ _.mkString("\t")}.mkString("\n")
      writeToFile(path,data)   
    }
  }
  
  implicit def indiv2Fitness[F](i: Individual[_,F]) = i.fitness
 
  def apply[G, F](g: G, e: G => F) = 
    new Individual[G, F] {
      val genome = g
      val fitness = e(g)
    }
  
   
}

trait Individual[+G, +F] {
  def genome: G
  def fitness: F
  
  override def toString = "(" + genome.toString + ", " + fitness.toString + ")"
}



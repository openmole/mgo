/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.breed

import fr.iscpif.mgo._
import java.util.Random

trait Breeding { this: Evolution =>
  
  def breed(archive: Population[G, MF], offSpringSize: Int)(implicit aprng: Random): IndexedSeq[G] = {

    //Crossover sur matingPopulation puis mutation
    def breed(acc: List[G] = List.empty): List[G] = {
      if (acc.size >= offSpringSize) acc
      else {
        val newIndividuals = crossover(
          selection(archive).genome,
          selection(archive).genome).
        map { mutate(_) }.take(offSpringSize).toIndexedSeq
        breed(acc ++ newIndividuals)
      }
    }

    breed().toIndexedSeq
  }
  
}

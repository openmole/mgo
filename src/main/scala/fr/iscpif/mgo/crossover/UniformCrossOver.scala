/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.crossover

import fr.iscpif.mgo._
import ga._
import java.util.Random
import fr.iscpif.mgo.tools.Random._


// TODO: Self-Adaptive pour crossover; à chercher ?
trait UniformCrossOver extends CrossOver { this: GAEvolution => 
    
  
  def crossoverRate: Double = 0.5

  def crossover (g1: G, g2: G) (implicit aprng : Random, factory: Factory[G]) = {
    // False on echange, true on maintient
    val rngValue = (0 until g1.content.size).map{x => !(aprng.nextDouble < crossoverRate)}
    val offspringValues = (rngValue zip (g1.content zip g2.content)) map {
      case (b, (g1e, g2e)) =>
        if(b) (g1e, g2e) else (g2e, g1e)
    }
    
    IndexedSeq(factory(offspringValues.map{_._1}),  factory(offspringValues.map{_._2}))
  }

}


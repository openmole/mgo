/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga.operators

import fr.iscpif.mgo._
import ga._
import java.util.Random
import fr.iscpif.mgo.tools.Random._


// TODO: Self-Adaptive pour crossover; à chercher ?
class RandomWrappedValuesCrossOver[G <: GAGenome, F <: GAGenomeFactory[G]](rate: Random => Double = _.nextDouble) extends CrossOver [G, F] {
    
  def this(rate: Double) = this( _ => rate)
  
  def apply (g1: G, g2: G, factory: F) (implicit aprng : Random) = {
    val crossoverRate = rate(aprng)
    
    // False on echange, true on maintient
    val rngValue = (0 until g1.wrappedValues.size).map{x => !(aprng.nextDouble < crossoverRate)}
    val offspringValues = (rngValue zip (g1.wrappedValues zip g2.wrappedValues)) map {
      case (b, (g1e, g2e)) =>
        if(b) (g1e, g2e) else (g2e, g1e)
    }
    
    IndexedSeq(factory(offspringValues.map{_._1}),  factory(offspringValues.map{_._2}))
  }

}


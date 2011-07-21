/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.mappedgenome.genomedouble

import org.openmole.tools.mgo.mappedgenome.genomedouble._
import org.apache.commons.math.util.FastMath._
import org.openmole.tools.mgo.model.Genome
import org.openmole.tools.mgo.model.GenomeFactory
import org.openmole.tools.mgo.model.SigmaParametersGenome
import org.openmole.tools.mgo.tools.Random._
import java.util.Random

class EvolvingSoftGaussianMutation[T <: Genome with SigmaParametersGenome, F <: GenomeFactory[T]] (interval: IntervalSet, rate: Random => Double = rng => rng.nextFloat) extends GenomeOperation[T, F] {
  
  override def operate(genomes: IndexedSeq[T],indices:List[Int])(implicit factory: F, rng: Random): T = {
    
    val selectedGenome = genomes.random 
    import selectedGenome._
    
    val newValues = subset(genomes.position,genomes.size) zip sigma map {case(g, s) => rng.nextGaussian * s  + g}
    return factory.apply(newValues)
    
  }
  
  def operateMutation(value:Double,min:Double,max:Double)(implicit rng: Random):Double= {
     //genomewithmetaparameter specialisation du genome actuel = genome + accesseur vers sigma, 
  //comme ca je garde un comportemenet generique
  //if = heritage et spécialisation 
  //genome metier wrap meta-genome
  
  //Faire evoluer le sigma qui correspond a cette valeur, donc il faut avoir autre chose que seulement la valeur ...  
  val mapOfBounds = IntervalSet.intervallDecorator.generateBounds(interval)(rng)  
    
  //on fait un ajustement fonction de sigma et mean
  //http://c-faq.com/lib/gaussian.html
  //http://www.developpez.net/forums/d331848/autres-langages/algorithmes/contribuez/generation-nombre-aleatoire-suivant-loi-gaussienne/
  val gRnd = rng.nextGaussian()
  val gRndAffine =  ( gRnd  * sigma ) + value
    
    return clamp(gRndAffine,min,max)
  }
  

  
  }

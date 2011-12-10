/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

//TODO: Créer pour maintenir la coherence entre les crossover GP qui renvoie un Genome, et les crossover GA qui en renvoie deux
abstract class GPCrossOver [G <: AbstractGenome, F <: GenomeFactory [G]]
  extends Operator [G, F, G] {
  }

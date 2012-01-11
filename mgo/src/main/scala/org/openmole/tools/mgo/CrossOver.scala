/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import java.util.Random

abstract class CrossOver [G <: Genome, F <: GenomeFactory [G]]
  extends Operator [G, F, IndexedSeq[G]] {
  }
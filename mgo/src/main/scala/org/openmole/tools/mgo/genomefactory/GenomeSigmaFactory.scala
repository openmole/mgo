/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.genomefactory

import org.openmole.tools.mgo.ga._
import org.openmole.tools.mgo._

trait GenomeSigmaFactory [G <: GAGenome with SigmaParameters] 
  extends GenomeFactory [G] 
  with FromValuesFactory [G]

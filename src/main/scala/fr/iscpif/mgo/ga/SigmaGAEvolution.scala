/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga


trait SigmaGAEvolution extends GAEvolution {
  type G <: GAGenome with Sigma
}

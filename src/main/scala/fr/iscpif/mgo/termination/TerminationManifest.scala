/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.termination

trait TerminationManifest { this: Termination =>
  implicit val stateManifest: Manifest[STATE]
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo

import java.util.Random

trait Factory[G <: Genome] {
  def random(implicit rng: Random): G
  def apply(t: G#T): G
  def apply(g: G, ops: G => G#T*): G = 
    ops.foldLeft(g)((g, op) => apply(op(g)))
}

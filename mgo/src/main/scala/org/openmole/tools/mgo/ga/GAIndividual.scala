/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.ga

import org.openmole.tools.mgo.Individual

trait GAIndividual[+G <: GAGenome, +F <: GAFitness] extends Individual[G, F]

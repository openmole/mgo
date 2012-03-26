/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.ga

import fr.iscpif.mgo.Individual

trait GAIndividual[+G <: GAGenome, +F <: GAFitness] extends Individual[G, F]

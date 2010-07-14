/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model;

/**
 *
 * @author salmamesmoudi
 */
public interface IPoint extends Comparable<IPoint>{
    Comparable getComparable(int dim);
    int getDim();
}

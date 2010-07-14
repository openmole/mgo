/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.diversity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Vector;
import org.openmole.tools.mgo.model.IPoint;
import org.openmole.tools.mgo.model.Point;

/**
 *
 * @author salmamesmoudi
 */
public class TestPoint extends Point {

    final private Double[] coordinates;

    public TestPoint(Double... coordinates) {
        this.coordinates = coordinates;
    }

   /* @Override
    public Iterable<? extends Comparable> getComparables() {
        return Arrays.asList(coordinates);
    }*/

    @Override
    public Double getComparable(int dim) {
        return coordinates[dim];
    }

    @Override
    public int getDim() {
        return coordinates.length;
    }

}

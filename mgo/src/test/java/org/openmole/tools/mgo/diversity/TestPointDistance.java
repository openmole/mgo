/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.diversity;

import org.openmole.tools.mgo.dominate.diversity.IDistanceComputing;

/**
 *
 * @author salmamesmoudi
 */
public class TestPointDistance implements IDistanceComputing<TestPoint> {

    @Override
    public double computeDistance(TestPoint p1, TestPoint p2) {

        if(p1.getDim() != p2.getDim()) throw new ArithmeticException("P1 and P2 doesn't have the same dimension " + p1.getDim() + " and " + p2.getDim() + ".");

        double distance = 0.0;
        for(int i = 0; i < p1.getDim(); i++) {
            distance += Math.pow(p1.getComparable(i) - p2.getComparable(i),2);
        }

        return Math.sqrt(distance);
    }

}

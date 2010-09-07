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

        if(p1.size() != p2.size()) throw new ArithmeticException("P1 and P2 doesn't have the same dimension " + p1.size() + " and " + p2.size() + ".");

        double distance = 0.0;
        for(int i = 0; i < p1.size(); i++) {
            distance += Math.pow(p1.apply(i) - p2.apply(i),2);
        }

        return Math.sqrt(distance);
    }

}

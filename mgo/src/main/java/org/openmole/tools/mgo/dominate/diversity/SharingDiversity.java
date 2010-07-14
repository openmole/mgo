/*
 *  Copyright (C) 2010 Salma Mesmoudi <salma.mesmoudi at openmole.org>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.openmole.tools.mgo.dominate.diversity;

import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;
import no.uib.cipr.matrix.UpperSymmPackMatrix;

/**
 *
 * @author salmamesmoudi
 */
public class SharingDiversity {

    public <T extends Comparable> Map<T, Double> setSimilarities(Vector<? extends T> vect, IDistanceComputing<T> distance, double alpha, double nicheSize) {

        Map<T, Double> ret = new TreeMap<T, Double>();
        //il faut la prendre comme valeur de densit?, celui qui a la plus grande valeur
        //est le plus repr?sent?

        UpperSymmPackMatrix dMatrix;
        MatriceDistances M = new MatriceDistances();
        dMatrix = M.matriceRemplissage(vect, distance);


        // compute similarities

        double sum = 0.0;

        for (int i = 0; i < vect.size(); i++) {
            sum = 0.0;
            for (int j = 0; j < vect.size(); j++) {
                sum += sh(dMatrix.get(i, j), alpha, nicheSize);
            }
            ret.put(vect.elementAt(i), sum);
        }
        return ret;

    }

    /**

     * Sharing function

     * @param dist the distance value

     */
    double sh(double dist, double alpha, double nicheSize) {
        double result;
        if (dist < nicheSize) {
            result = 1.0 - Math.pow(dist / nicheSize, alpha);
        } else {
            result = 0.0;
        }

        return result;

    }
}

/*
 *  Copyright (C) 2010 Romain Reuillon <romain.reuillon at openmole.org>
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


import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.openmole.tools.mgo.model.IPoint;
import org.openmole.tools.mgo.model.IOperation;
import org.openmole.tools.mgo.model.Operations;
import org.openmole.tools.mgo.tools.OrderPointOneDim;

/**
 *
 * @author salmamesmoudi
 */
public class Crowding {

    public <T extends IPoint, T2> List<T> orderByDecreasingCroding(Collection<T> points, int dim) {
        return orderByDecreasingCroding(points, dim, Operations.BaseOperations);

    }

    public <T extends IPoint> List<T> orderByDecreasingCroding(Collection<T> points, int dim, Map<Class, IOperation> operations) {

        List<T> ret = new ArrayList<T>(points.size());

        if (points.size() <= 2) {
            for (T point : points) {
                ret.add(point);
            }
            return ret;
        } else {

            class CrowdingInfo implements IPoint {
                T point;
                Double crowding;

                public CrowdingInfo(T point, Double crowding) {
                    this.point = point;
                    this.crowding = crowding;
                }

              /*  @Override
                public Iterable<? extends Comparable> getComparables() {
                    return point.getComparables();
                }*/

                @Override
                public Comparable getComparable(int dim) {
                    return point.getComparable(dim);
                }

                @Override
                public int getDim() {
                    return point.getDim();
                }

                @Override
                public int compareTo(IPoint t) {
                    return point.compareTo(((CrowdingInfo)t).point);
                }

               /* @Override
                public int compareTo(CrowdingInfo t) {
                    return Double.compare(t.crowding, crowding);
                }*/


            }
            ArrayList<CrowdingInfo> crowding = new ArrayList<CrowdingInfo>(points.size());

            //Map<T, Double> crowding = new TreeMap<T, Double>();

            // set diversity to 0
            int i = 0;
            for (T point : points) {
                crowding.add(i++, new CrowdingInfo(point, 0.0));
                //crowding.put(point, 0.0);
            }

            // for each objective
            for (int obj = 0; obj < dim; obj++) {
                //trier selon l'objectif

                OrderPointOneDim.order(obj, crowding);
               // OrderPointOneDim<T> opod = new OrderPointOneDim<T>(obj);
               // opod.addAll(points);

                CrowdingInfo firstCrowdingInfo = crowding.get(0);
                CrowdingInfo lastCrowdingInfo = crowding.get(crowding.size() - 1);

                T first = firstCrowdingInfo.point;
                T last = lastCrowdingInfo.point;

                Object min = first.getComparable(obj);
                Object max = last.getComparable(obj);

                firstCrowdingInfo.crowding = Double.POSITIVE_INFINITY;
                lastCrowdingInfo.crowding = Double.POSITIVE_INFINITY;


               // System.out.println(min.getClass().getName());

                IOperation operation = operations.get(min.getClass());
                if(operation == null) throw new ArithmeticException("Operation not registred for class " + min.getClass().getName());

                Object maxMinusMin = operation.substract(max, min);

                Iterator<CrowdingInfo> itOpod = crowding.iterator();
                CrowdingInfo ptMinus1 = itOpod.next();
                CrowdingInfo pt = itOpod.next();

                while (itOpod.hasNext()) {
                    CrowdingInfo ptPlus1 = itOpod.next();
                    double distance = operation.divide(operation.substract(ptPlus1.getComparable(obj), ptMinus1.getComparable(obj)), maxMinusMin);
                    pt.crowding += distance;
  
                    ptMinus1 = pt;
                    pt = ptPlus1;
                }
            }

           

            /*CrowdingInfo[] toSort = new CrowdingInfo[crowding.size()];

            int i = 0;
            for(Map.Entry<T,Double> entry : crowding.entrySet()) {
                toSort[i] = new CrowdingInfo(entry.getKey(), entry.getValue());
                i++;
            }*/

            Collections.sort(crowding, new Comparator<CrowdingInfo>() {

                @Override
                public int compare(CrowdingInfo t, CrowdingInfo t1) {
                    return Double.compare(t.crowding, t1.crowding);
                }

            });

            for(CrowdingInfo elt : crowding) {
                ret.add(elt.point);
            }

            return ret;
        }

    }
    // }
}

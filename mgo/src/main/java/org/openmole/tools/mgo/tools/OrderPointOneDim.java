/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openmole.tools.mgo.tools;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;
import org.openmole.tools.mgo.model.IPoint;

/**
 *
 * @author salmamesmoudi
 */
public class OrderPointOneDim {



    /*class ComparablePoint implements Comparable<ComparablePoint> {

        T point;
        Comparable coordonnee;

        public ComparablePoint(T point, Comparable coordonnee) {
            this.point = point;
            this.coordonnee = coordonnee;
        }

        @Override
        public int compareTo(ComparablePoint t) {
            int compare = coordonnee.compareTo(t.coordonnee);
            if(compare != 0) return compare;
            return point.compareTo(t.point);
        }
    }*/


    //private final TreeSet<ComparablePoint> set = new TreeSet<ComparablePoint>();
    //private final int dim;
   // final private ArrayList<T> ordered;

    public static void order(final int dim, ArrayList<? extends IPoint> toOrder) {
        //this.dim = dim;
        Collections.sort(toOrder, new Comparator<IPoint>() {

            @Override
            public int compare(IPoint t, IPoint t1) {
                 int compare = t.getComparable(dim).compareTo(t1.getComparable(dim));
                 if(compare != 0) return compare;
                 return t.compareTo(t1);
            }

        });
       /*

        this.ordered = new ArrayList<T>(toOrder.size());

        int i = 0;
        for(T t: toOrder) {
            ordered
        }*/
    }

    /*public void insertPoint(T point) {
        Comparable coord = point.getComparable(dim);
        ComparablePoint toInsert = new ComparablePoint(point, coord);
        set.add(toInsert);
    }

    public void addAll(Iterable<T> points) {
        for (T p : points) {
            insertPoint(p);
        }
    }*/

    /*@Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {

            Iterator<ComparablePoint> comparablePointIt = set.iterator();

            @Override
            public boolean hasNext() {
                return comparablePointIt.hasNext();
            }

            @Override
            public T next() {
                return comparablePointIt.next().point;
            }

            @Override
            public void remove() {
                comparablePointIt.remove();
            }
        };
    }

    public T last() {
        return set.last().point;
    }

    public T first() {
        return set.first().point;
    }*/

    
}

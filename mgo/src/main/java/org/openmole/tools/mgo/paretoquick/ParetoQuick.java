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
package org.openmole.tools.mgo.paretoquick;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import org.openmole.tools.mgo.dominate.DominateMinimumStrict;
import org.openmole.tools.mgo.model.IPoint;
import org.openmole.tools.mgo.dominate.IDominate;
import org.openmole.tools.mgo.tools.OrderPointOneDim;

/**
 *
 * @author salmamesmoudi
 */
public class ParetoQuick {

    final static IDominate dominate = new DominateMinimumStrict();

    public <T extends IPoint> Collection<T> pareto(Collection<T> points){
        if(points.size() == 0) return points;
        return pareto(points, points.iterator().next().getDim());
    }
    
    public <T extends IPoint> Collection<T> pareto(Collection<T> points, int dim) {

        int curDim = dim - 1;

        if (points.isEmpty()) {
            return new ArrayList<T>(0);
        }
        if (curDim == 0) {
            Iterator<T> it = points.iterator();
            T ret = it.next();

            while (it.hasNext()) {
                T elt = it.next();
                if (dominate.isDominated(ret, elt)) {
                    ret = elt;
                }
            }

            ArrayList<T> retVec = new ArrayList<T>(1);
            retVec.add(ret);
            return retVec;
        }

        Queue<ArrayList<T>> toProceed = new LinkedList<ArrayList<T>>();
        Iterator<T> it = points.iterator();

        while (it.hasNext()) {
            ArrayList<T> vect = new ArrayList<T>(2);
            vect.add(it.next());

            if (it.hasNext()) {
                vect.add(it.next());
            }
            toProceed.add(vect);
        }

        ArrayList<T> archive = toProceed.remove();

        while (!toProceed.isEmpty()) {
            Collection<T> archive2 = eliminer(toProceed.remove(), curDim);
            ArrayList<T> archive2Array = new ArrayList<T>(archive2.size());
            archive2Array.addAll(archive2);

            ArrayList<T> merged = new ArrayList<T>(archive.size() + archive2.size());
            merged.addAll(eliminer(archive, curDim));
            merged.addAll(eliminer(archive2Array, curDim));
            toProceed.add(merged);

            archive = toProceed.remove();
        }


        return eliminer(archive, curDim);
    }

    public <T extends IPoint> void archivePreto2D(List<T> vect, List<T> archive) {

       // Logger.getLogger(ParetoQuick.class.getName()).info(vect.toString());

        if (vect.size() == 0) {
            return;
        }

        Iterator<T> it = vect.iterator();

        T elt = it.next();
        archive.add(elt);

        Comparable min = elt.getComparable(0);
        //T second = elt;



        while (it.hasNext()) {
            elt = it.next();
            if ( (min.compareTo(elt.getComparable(0)) >= 0)) { /* || (second.getComparable(1).compareTo(elt.getComparable(1)) == 0)) {*/
             //   Logger.getLogger(ParetoQuick.class.getName()).info(elt.toString());

                min = elt.getComparable(0);
                archive.add(elt);
               // second = elt;
            } 
        }
    }

    public <T extends IPoint> Collection<T> eliminer(ArrayList<T> v, int curDim) {
        if (v.isEmpty()) {
            return Collections.EMPTY_LIST;
        }
        List<T> archive = new LinkedList<T>();

        if (v.size() == 2) {
            Iterator<T> it = v.iterator();

            T p1 = it.next();
            T p2 = it.next();

            switch (dominate.dominated(p1, p2)) {
                case LEFT:
                    archive.add(p2);
          //          Logger.getLogger(ParetoQuick.class.getName()).info("Elimine " + p1.toString() + " against " + p2.toString());
                    break;
                case RIGHT:
                    archive.add(p1);
           //         Logger.getLogger(ParetoQuick.class.getName()).info("Elimine " + p2.toString() + " against " + p1.toString());
                    break;
                case NONE:
                    archive.add(p1);
                    archive.add(p2);
                    break;
            }
            return archive;
        }

        OrderPointOneDim.order(curDim, v);

        if (curDim == 1) {
            archivePreto2D(v, archive);
            return archive;
        }


        int half = v.size() / 2;

        Iterator<T> it = v.iterator();

        //  Vector<TaggedPoint> v1 = new Vector<TaggedPoint>(half);
        //  Vector<TaggedPoint> v2 = new Vector<TaggedPoint>(half + 1);
        ArrayList<TaggedPoint> vTagged = new ArrayList<TaggedPoint>(v.size());

        for (int i = 0; i < half; i++) {
            vTagged.add(new TaggedPoint(it.next(), Tag.A));
        }

        for (int i = half; i < v.size(); i++) {
            vTagged.add(new TaggedPoint(it.next(), Tag.B));
        }

        OrderPointOneDim.order(curDim - 1, vTagged);
       /* orderTagged.addAll(v1);
        orderTagged.addAll(v2);*/

       /* Vector<TaggedPoint> merge = new Vector<TaggedPoint>(v1.size() + v2.size());

        for (TaggedPoint tp : orderTagged) {
            merge.add(tp);
        }*/


        Collection<T> procV1 = eliminerP(vTagged.subList(0, vTagged.size() / 2), archive);
        Collection<T> procV2 = eliminerNP(vTagged.subList(vTagged.size() / 2, vTagged.size()), archive);

        v = new ArrayList<T>(procV1.size() + procV2.size());

        v.addAll(procV1);
        v.addAll(procV2);

        archive.addAll(eliminer(v, curDim - 1));

        return archive;
    }

    <T extends IPoint> Collection<T> eliminerNP(Collection<TaggedPoint> points, Collection<T> archive) {
        Collection<T> ret = new LinkedList<T>();

        //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
        //l'objectif de cette classe est d'???limner les Point du 2i???me vecteur qui sont domin???s par le 1ier vecteur
        // les points qui restent du 2i???me vecteur dans le 1er vecteur sont envoy???s dans l'archive pareto et ???liminer du vecteur en cour


        for (TaggedPoint<T> p : points) {
            if (p.getTag() == Tag.B) {
                ret.add(p.getPoint());
            } else {
                archive.add(p.getPoint());
            }
        }

        return ret;
    }

    <T extends IPoint> Collection<T> eliminerP(Collection<TaggedPoint> points, Collection<T> archive) {
        Collection<T> ret = new LinkedList<T>();
        Collection<T> retA = new LinkedList<T>();
        //cette classe prend un vecteur de Point dont l'origine vient des deux vecteurs parents
        //l'objectif est de cette classe est d'???liminer les Point du 1i???me vecteur qui se trouvent dans le 2ieme vecteur

        for (TaggedPoint<T> p : points) {
            if (p.getTag() == Tag.B) {
                ret.add(p.getPoint());
            } else {
                retA.add(p.getPoint());
            }
        }

        for (T p : ret) {
            if (dominatePointList(p, retA)) {
                archive.add(p);
            }

        }

        return retA;
    }

    boolean dominatePointList(IPoint p, Collection<? extends IPoint> v) {

        for (IPoint p1 : v) {
            if (dominate.isDominated(p, p1)) {
                return false;
            }
        }
        return true;

    }
}

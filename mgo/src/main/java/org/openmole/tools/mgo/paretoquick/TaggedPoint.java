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

import org.openmole.tools.mgo.model.IPoint;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class TaggedPoint<T extends IPoint> implements  IPoint {

    final T point;
    final Tag tag;

    public TaggedPoint(T point, Tag tag) {
        this.point = point;
        this.tag = tag;
    }

    public T getPoint() {
        return point;
    }

    public Tag getTag() {
        return tag;
    }

    @Override
    public String toString() {
        return point.toString() + " " + tag.toString();
    }

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
        TaggedPoint tp = (TaggedPoint) t;
        return point.compareTo(tp.point);
    }

  /*  @Override
    public Iterable<? extends Comparable> getComparables() {
        return point.getComparables();
    }*/



}

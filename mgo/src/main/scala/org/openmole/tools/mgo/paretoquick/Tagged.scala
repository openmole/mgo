/*
 * Copyright (C) 2010 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.paretoquick

import org.openmole.tools.mgo.model.MultiGoal


class Tagged[P <: MultiGoal](val multiGoal: P, val tag: Tag.Value) extends MultiGoal(multiGoal.goals) {

  override def toString = goals.toString + " " + tag.toString
  
    /*@Override
    public Comparable apply(int dim) {
        return point.apply(dim);
    }

    @Override
    public int size() {
        return point.size();
    }

    @Override
    public int compareTo(IPoint t) {
        TaggedPoint tp = (TaggedPoint) t;
        return point.compareTo(tp.point);
    }*/

}

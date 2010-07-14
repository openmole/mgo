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
package org.openmole.tools.mgo.dominate;

import org.openmole.tools.mgo.model.IPoint;

/**
 *
 * @author salmamesmoudi
 */
public class DominateMinimumStrict extends Dominate {
  
    
    @Override
    public boolean isDominated(IPoint p1, IPoint p2) {
        for(int i = 0; i < p1.getDim(); i++) {
            if(p1.getComparable(i).compareTo(p2.getComparable(i)) <= 0 )
                return false;
        }
        return true; 
    }



    @Override
    public DominateType dominated (IPoint p1, IPoint p2) {
        int a1 = 0 , a2 = 0 ,tot = 0;



       // Iterator<? extends Comparable> p1CoordIt = p1.getComparables().iterator();
       // Iterator<? extends Comparable> p2CoordIt = p2.getComparables().iterator();

        for(int i = 0; i < p1.getDim(); i++) {
            Comparable p1Coord = p1.getComparable(i);
            Comparable p2Coord = p2.getComparable(i);

            int compare = p1Coord.compareTo(p2Coord);

            if(compare < 0)
                a1++;
            else if(compare > 0)
                a2++;

            tot++;

            if(a1 != tot && a2 != tot) return DominateType.NONE;
        }

       // if(p1CoordIt.hasNext() != p2CoordIt.hasNext()) return DominateType.NONE;

        if(a1==tot)
            return DominateType.RIGHT;
        else if(a2==tot)
            return DominateType.LEFT;
        else
            return DominateType.NONE;
    }

}

/*
 *  Copyright (C) 2010 reuillon
 * 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of the
 *  License, or (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the Affero GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.statistic;

import java.util.ArrayList;
import java.util.Iterator;
import org.openmole.tools.mgo.model.IPoint;

/**
 *
 * @author reuillon
 */
public class Statistic {
    
    final ArrayList<Comparable> min;
    final ArrayList<Comparable> max;
    
    public Statistic(Iterable<? extends IPoint> goals) {
        Iterator<? extends IPoint> it = goals.iterator();
        IPoint first = it.next();
        
        min = new ArrayList<Comparable>(first.size());
        max = new ArrayList<Comparable>(first.size());
        
        for(int i = 0 ; i < first.size() ; i++) {
            min.add(i, first.apply(i));
            max.add(i, first.apply(i));
        }
        
        while(it.hasNext()) {
            IPoint current = it.next();
            
            for(int i = 0 ; i < current.size(); i++) {
                Comparable comparable = current.apply(i);

                
                if(comparable.compareTo(min.get(i)) < 0) {
                    min.set(i, comparable);
                } else if (comparable.compareTo(max.get(i)) > 0) {
                    max.set(i, comparable);
                }
            }
        }
    }
    
    public Iterable<Comparable> getMin() {
        return min;
    }
    
    public Iterable<Comparable> getMax() {
        return max;
    }
    
}

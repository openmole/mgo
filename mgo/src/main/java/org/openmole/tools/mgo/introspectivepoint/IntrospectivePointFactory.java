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

package org.openmole.tools.mgo.introspectivepoint;

import java.lang.reflect.Field;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections15.map.ReferenceMap;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class IntrospectivePointFactory {

    final static private IntrospectivePointFactory instance = new IntrospectivePointFactory();

    static public IntrospectivePointFactory getInstance() {
        return instance;
    }
    
    Map<Class, Field[]> cache = new ReferenceMap<Class, Field[]>(ReferenceMap.SOFT, ReferenceMap.SOFT);

   synchronized public <T> IntrospectivePoint<T> buildIntrospectivePoint(T object) throws IllegalArgumentException, IllegalAccessException {
        Class originClass = object.getClass();
        Field[] ret = cache.get(originClass);

        if(ret == null) {
            List<Field> fields = new LinkedList<Field>();
            Class curClass = originClass;

            while(curClass != Object.class) {
                for(Field f : curClass.getDeclaredFields()) {
                    if(f.isAnnotationPresent(Coordinate.class)) {
                        f.setAccessible(true);
                        fields.add(f);
                    }
                }
                curClass = curClass.getSuperclass();
            }
            ret = fields.toArray(new Field[fields.size()]);
            cache.put(originClass, ret);
        }

        Comparable[] comp = new Comparable[ret.length];
        int i = 0;

        for(Field f: ret) {
            comp[i] = (Comparable) f.get(object);
            i++;
        }

        return new IntrospectivePoint<T>(object, comp);
    }
}

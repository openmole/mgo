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

package org.openmole.tools.mgo.model;

import org.openmole.tools.mgo.introspectivepoint.IntrospectivePoint;
import org.openmole.tools.mgo.introspectivepoint.IntrospectivePointFactory;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class IntrospectionGoalIndividu<T, G> extends Individu<T, IntrospectivePoint<G> > {

    public IntrospectionGoalIndividu(T genome, IntrospectivePoint<G> goal) {
        super(genome, goal);
    }

    public IntrospectionGoalIndividu(T genome, G goal) throws IllegalArgumentException, IllegalAccessException {
        this(genome, IntrospectivePointFactory.getInstance().buildIntrospectivePoint(goal));
    }

   public G getOriginalGoal() {
       return getGoal().getObject();
   }

}

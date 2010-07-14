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

package org.openmole.tools.mgo.dominate.migration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import org.openmole.tools.distrng.prng.IPRNG;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Shuffle {
    public static <T> Collection<? extends Collection<? extends T>> shuffle(Collection<? extends Collection<? extends T>> populations, IPRNG prng) {
    //implicit m: Manifest[T]
    int nbIndividuals = 0;

    for(Collection population: populations) {
        nbIndividuals += population.size();
    }

    ArrayList<T> allIndividuals = new ArrayList<T>(nbIndividuals);

    for(Collection<? extends T> population: populations) {
        for(T individual: population) {
            allIndividuals.add(individual);
        }
    }

    for(int j = allIndividuals.size() - 1; j >= 0; j--) {
        int switchWith = prng.nextInt(j + 1);
        T tmp = allIndividuals.get(switchWith);
        allIndividuals.set(switchWith, allIndividuals.get(j));
        allIndividuals.set(j, tmp);
    }

    ArrayList<LinkedList<T>> ret = new ArrayList<LinkedList<T>>(populations.size());

    for(int j = 0; j < populations.size(); j++) {
        ret.add(new LinkedList<T>());
    }

    for(T individual: allIndividuals) {
        int selected = prng.nextInt(populations.size());
        ret.get(selected).add(individual);
    }

    return ret;
  }

}

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Population<G, GO extends IPoint, T extends IIndividu<? extends G, ? extends GO>> implements IPopulation<G,GO,T> {
    
    final Collection<T> population;

    public Population(Collection<T> population) {
        this.population = population;
    }

    public <T> T[] toArray(T[] ts) {
        return population.toArray(ts);
    }

    public Object[] toArray() {
        return population.toArray();
    }

    public int size() {
        return population.size();
    }

    public boolean retainAll(Collection<?> clctn) {
        return population.retainAll(clctn);
    }

    public boolean removeAll(Collection<?> clctn) {
        return population.removeAll(clctn);
    }

    public boolean remove(Object o) {
        return population.remove(o);
    }

    public Iterator<T> iterator() {
        return population.iterator();
    }

    public boolean isEmpty() {
        return population.isEmpty();
    }

    public int hashCode() {
        return population.hashCode();
    }

    public boolean equals(Object o) {
        return population.equals(o);
    }

    public boolean containsAll(Collection<?> clctn) {
        return population.containsAll(clctn);
    }

    public boolean contains(Object o) {
        return population.contains(o);
    }

    public void clear() {
        population.clear();
    }

    public boolean addAll(Collection<? extends T> clctn) {
        return population.addAll(clctn);
    }

    public boolean add(T e) {
        return population.add(e);
    }



    @Override
    public IGenomes<G> toGenomes() {
        IGenomes<G> ret = new Genomes<G>(population.size());

        for(IIndividu<? extends G, ? extends GO> indiv : population) {
            ret.add(indiv.getGenome());
        }

        return ret;
    }

    @Override
    public Collection<GO> toGoals() {
        Collection<GO> ret = new ArrayList<GO>(population.size());

         for(IIndividu<? extends G, ? extends GO> indiv : population) {
            ret.add(indiv.getGoal());
        }

        return ret;
    }
    
}

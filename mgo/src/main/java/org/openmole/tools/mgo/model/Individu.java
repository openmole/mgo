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


/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Individu<T, G extends IPoint> implements IIndividu<T, G>, IPoint {

    volatile static int curId = 0;

    final T genome;
    final G goal;
    final int id;

    public Individu(T genome, G goal) {
        this.genome = genome;
        this.goal = goal;
        id = curId++;
    }

    @Override
    public T getGenome() {
        return genome;
    }

    @Override
    public G getGoal() {
        return goal;
    }

    @Override
    public int getDim() {
        return goal.getDim();
    }

    @Override
    public Comparable getComparable(int dim) {
        return goal.getComparable(dim);
    }

    @Override
    public int compareTo(IPoint o) {
        Individu<T,G> indiv = (Individu<T,G>) o;
        int compare = getGoal().compareTo(indiv.getGoal());
        if(compare != 0) return compare;
        return id - indiv.id;
    }

    @Override
    public String toString() {
        StringBuilder toString = new StringBuilder();
        toString.append(getGoal().toString());
        toString.append(' ');
        toString.append(getGenome().toString());

        return toString.toString();
    }



}

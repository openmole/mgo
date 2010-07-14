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
import java.util.List;
import java.util.ListIterator;
import java.util.RandomAccess;
import org.openmole.tools.distrng.prng.IPRNG;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Genomes<T> implements IGenomes<T>, RandomAccess {
    final private ArrayList<T> genomes;

    public Genomes(ArrayList<T> genomes) {
        this.genomes = genomes;
    }

    public Genomes(int size) {
        this.genomes = new ArrayList<T>(size);
    }

    public String toString() {
        return genomes.toString();
    }

    public boolean retainAll(Collection<?> c) {
        return genomes.retainAll(c);
    }

    public boolean removeAll(Collection<?> c) {
        return genomes.removeAll(c);
    }

    public boolean containsAll(Collection<?> c) {
        return genomes.containsAll(c);
    }

    public List<T> subList(int fromIndex, int toIndex) {
        return genomes.subList(fromIndex, toIndex);
    }

    public ListIterator<T> listIterator(int index) {
        return genomes.listIterator(index);
    }

    public ListIterator<T> listIterator() {
        return genomes.listIterator();
    }

    public Iterator<T> iterator() {
        return genomes.iterator();
    }

    public int hashCode() {
        return genomes.hashCode();
    }

    public boolean equals(Object o) {
        return genomes.equals(o);
    }

    public void trimToSize() {
        genomes.trimToSize();
    }

    public <T> T[] toArray(T[] a) {
        return genomes.toArray(a);
    }

    public Object[] toArray() {
        return genomes.toArray();
    }

    public int size() {
        return genomes.size();
    }

    public T set(int index, T element) {
        return genomes.set(index, element);
    }

    public boolean remove(Object o) {
        return genomes.remove(o);
    }

    public T remove(int index) {
        return genomes.remove(index);
    }

    public int lastIndexOf(Object o) {
        return genomes.lastIndexOf(o);
    }

    public boolean isEmpty() {
        return genomes.isEmpty();
    }

    public int indexOf(Object o) {
        return genomes.indexOf(o);
    }

    public T get(int index) {
        return genomes.get(index);
    }

    public void ensureCapacity(int minCapacity) {
        genomes.ensureCapacity(minCapacity);
    }

    public boolean contains(Object o) {
        return genomes.contains(o);
    }

    public Object clone() {
        return genomes.clone();
    }

    public void clear() {
        genomes.clear();
    }

    public boolean addAll(int index, Collection<? extends T> c) {
        return genomes.addAll(index, c);
    }

    public boolean addAll(Collection<? extends T> c) {
        return genomes.addAll(c);
    }

    public void add(int index, T element) {
        genomes.add(index, element);
    }

    public boolean add(T e) {
        return genomes.add(e);
    }

    @Override
    public T getRandomGenome(IPRNG prng) {
        return get(prng.nextInt(0, size()));
    }
    

}

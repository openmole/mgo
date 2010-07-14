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

package fr.iscpif.tsp;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import org.openmole.tools.distrng.prng.IPRNG;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class SegmentMutation implements IGenomeOperation {

    @Override
    public Genome operate(Individu[] population, IPRNG rng) {
        Genome from = population[rng.nextInt(population.length)].getGenome();
        float[] values = from.getValues();
        int length = rng.nextInt(values.length / 2);

        List<Float> tmpValues = new LinkedList<Float>();

        for(float value: values) {
            tmpValues.add(value);
        }
        
        int firstPos = rng.nextInt(tmpValues.size() - length);

        ListIterator<Float> itTmp = tmpValues.listIterator(firstPos);
        List<Float> first = new LinkedList<Float>();

        for(int i = 0; i < length; i++) {
            Float elt = itTmp.next();
            first.add(elt);
            itTmp.remove();
        }

        int secondPos = rng.nextInt(tmpValues.size() - length);
        itTmp = tmpValues.listIterator(secondPos);
        List<Float> second = new LinkedList<Float>();

        for(int i = 0; i < length; i++) {
            Float elt = itTmp.next();
            second.add(elt);
            itTmp.remove();
        }

        for(Float f: first) {
            itTmp.add(f);
        }
       
        itTmp = tmpValues.listIterator(firstPos);

        for(Float f: second) {
            itTmp.add(f);
        }

        float[] ret = new float[tmpValues.size()];
        int i = 0;
        for(Float f: tmpValues) {
            ret[i++] = f;
        }

        return new Genome(ret);
    }

}

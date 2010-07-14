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

import org.openmole.tools.distrng.prng.IPRNG;
import org.openmole.tools.mgo.type.IGenomes;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class RandomMutation implements IGenomeOperation {

   /* final double rate;

    public RandomMutation(double rate) {
        this.rate = rate;
    }*/

    @Override
    public Genome operate(Individu[] ig, IPRNG prng) {
        float rate = prng.nextFloat();
        Genome from = ig[prng.nextInt(ig.length)].getGenome();

        float[] values = new float[from.getValues().length];

        for (int i = 0; i < from.getValues().length; i++) {
            if (prng.nextDouble() < rate) {
                values[i] = prng.nextFloat();
            } else {
                values[i] = from.getValues()[i];
            }
        }
        return new Genome(values);

    }
}

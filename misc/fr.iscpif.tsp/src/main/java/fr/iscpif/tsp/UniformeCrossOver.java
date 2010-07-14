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

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class UniformeCrossOver implements IGenomeOperation {

    @Override
    public Genome operate(Individu[] ig, IPRNG prng) {
         Genome from1 = ig[prng.nextInt(ig.length)].getGenome();
         Genome from2 = ig[prng.nextInt(ig.length)].getGenome();

        float values[] = new float[from1.getValues().length];

        for (int i = 0; i < from1.getValues().length; i++) {
            if (prng.nextDouble() < 0.5) {
                values[i] = from1.getValues()[i];
            } else {
                values[i] = from2.getValues()[i];
            }
        }

        return new Genome(values);
    }
}

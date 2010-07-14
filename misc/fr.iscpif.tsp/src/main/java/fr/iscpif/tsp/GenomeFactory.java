/*
 *  Copyright (C) 2010 reuillon
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

import java.util.ArrayList;
import org.openmole.tools.distrng.prng.IPRNG;
import org.openmole.tools.mgo.type.Genomes;
import org.openmole.tools.mgo.type.IGenomes;

/**
 *
 * @author reuillon
 */
public class GenomeFactory {

    final static private GenomeFactory instance = new GenomeFactory();

    public static GenomeFactory getInstance() {
        return instance;
    }

    public Genome randomGenome(int length, IPRNG rng) {
        float[] values = new float[length];

        for(int i = 0; i < length; i++) {
            values[i] = rng.nextFloat();
        }

        return new Genome(values);
    }

   public IGenomes<Genome> randomGenomes(int length, int nb, IPRNG rng) {
        ArrayList<Genome> ret = new ArrayList<Genome>(nb);

        for(int i = 0; i < nb; i++) {
            ret.add(randomGenome(length, rng));
        }

        return new Genomes<Genome>(ret);
    }
}

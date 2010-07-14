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
package org.openmole.tools.mgo.evolution;

import java.util.ArrayList;
import org.openmole.tools.distrng.prng.IPRNG;
import org.openmole.tools.mgo.model.Genomes;
import org.openmole.tools.mgo.model.IGenomes;
import org.openmole.tools.mgo.model.IPopulation;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class EvolutionEngine<T> {

    ArrayList<IGenomeOperation<T>> operations = new ArrayList<IGenomeOperation<T>>();

    public void addOperation(IGenomeOperation<T> operation, int nb) {
        for (int i = 0; i < nb; i++) {
            operations.add(operation);
        }
    }

    public void addOperation(IGenomeOperation<T> operation) {
        addOperation(operation, 1);
    }

    public T makeEvolve(IGenomes<T> genomes, IPRNG rng) {
        IGenomeOperation<T> operation = operations.get(rng.nextInt(0, operations.size()));
        return operation.operate(genomes, rng);
    }

    public IGenomes<T> makeEvolve(IGenomes<T> genomes, int add, IPRNG rng) {
        return makeEvolve(genomes, add, rng, NoSelection$.MODULE$);
    }


    public IGenomes<T> makeEvolve(IGenomes<T> genomes, int add, IPRNG rng, ISelection<? super T> selection) {
        ArrayList<T> ret = new ArrayList<T>(add);
        int i = 0;

        while (i < add) {
            IGenomeOperation<T> operation = operations.get(rng.nextInt(0, operations.size()));

            T newGenome = operation.operate(genomes, rng);

            if(selection.accept(newGenome)) {
                ret.add(newGenome);
                i++;
            }
        }

        return new Genomes<T>(ret);
    }

    public IGenomes<T> makeEvolve(IPopulation<T, ?, ?> population, int add, IPRNG rng, ISelection<? super T> selection) {
        IGenomes<T> genomes = population.toGenomes();
        return makeEvolve(genomes, add, rng,selection);
    }

    public IGenomes<T> makeEvolve(IPopulation<T, ?, ?> population, int add, IPRNG rng) {
        IGenomes<T> genomes = population.toGenomes();
        return makeEvolve(genomes, add, rng);
    }
}

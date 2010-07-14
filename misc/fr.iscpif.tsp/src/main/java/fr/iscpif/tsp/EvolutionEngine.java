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

import java.util.ArrayList;
import org.openmole.tools.distrng.prng.IPRNG;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class EvolutionEngine {

    ArrayList<IGenomeOperation> operations = new ArrayList<IGenomeOperation>();

    public void addOperation(IGenomeOperation operation, int nb) {
        for (int i = 0; i < nb; i++) {
            operations.add(operation);
        }
    }

    public void addOperation(IGenomeOperation operation) {
        addOperation(operation, 1);
    }

    public Genome makeEvolve(Individu[] population, IPRNG rng) {
        IGenomeOperation operation = operations.get(rng.nextInt(operations.size()));
        return operation.operate(population, rng);
    }
    
}

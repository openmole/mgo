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
package org.openmole.tools.mgo.dominate.diversity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeSet;
import org.openmole.tools.mgo.dominate.DominateMinimumStrict;
import org.openmole.tools.mgo.dominate.IDominate;
import org.openmole.tools.mgo.paretoquick.ParetoQuick;
import org.openmole.tools.mgo.model.IPoint;
import org.openmole.tools.mgo.model.IOperation;
import org.openmole.tools.mgo.model.Operations;

/**
 *
 * @author salmamesmoudi
 */
public class FitnessByRank {

    final private Crowding crowding = new Crowding();
    final static IDominate dominate = new DominateMinimumStrict();

    public <T extends IPoint> Collection<T> selectByFitnessAndCrowdingPareto(Collection<T> toSelect, int resPopSize) {
        if(toSelect.isEmpty()) return toSelect;
        return selectByFitnessAndCrowdingPareto(toSelect, toSelect.iterator().next().size(), resPopSize, Operations.BaseOperations);
    }

    public <T extends IPoint> Collection<T> selectByFitnessAndCrowdingPareto(Collection<T> toSelect,int dim, int resPopSize) {
        return selectByFitnessAndCrowdingPareto(toSelect, dim, resPopSize, Operations.BaseOperations);
    }

    public <T extends IPoint> Collection<T> selectByFitnessAndCrowding(Collection<T> toSelect, int resPopSize) {
        if(toSelect.isEmpty()) return toSelect;
        return selectByFitnessAndCrowding(toSelect, toSelect.iterator().next().size(), resPopSize);
    }

    public <T extends IPoint> Collection<T> selectByFitnessAndCrowding(Collection<T> toSelect,int dim, int resPopSize) {
        return selectByFitnessAndCrowding(toSelect, dim, resPopSize, Operations.BaseOperations);
    }
    public <T extends IPoint> Collection<T> selectByFitnessAndCrowdingPareto(Collection<T> toSelect, int dim,  int resPopSize, Map<Class, IOperation> operations) {

        ParetoQuick paretoQuick = new ParetoQuick();
        TreeSet<T> all = new TreeSet<T>();
        all.addAll(toSelect);

        Collection<T> ret = new LinkedList<T>();
        Collection<T> pareto = paretoQuick.pareto(all, dim);
    
        while(resPopSize > ret.size() + pareto.size()) {
            //Logger.getLogger(FitnessByRank.class.getName()).info("Size pareto: " + pareto.size() + " / " + all.size());

            ret.addAll(pareto);
            all.removeAll(pareto);
            pareto = paretoQuick.pareto(all, dim);
        }

       // Logger.getLogger(FitnessByRank.class.getName()).info("Size pareto: " + pareto.size()+ " / " + all.size());

        for(T point : crowding.orderByDecreasingCroding(pareto, dim, operations)) {
            ret.add(point);
            if(ret.size() >= resPopSize) break;
        }
        // Logger.getLogger(FitnessByRank.class.getName()).info("Size: " + ret.size());

        return ret;
    }

    public <T extends IPoint> Collection<T> selectByFitnessAndCrowding(Collection<T> toSelect, int dim,  int resPopSize, Map<Class, IOperation> operations) {

        if(toSelect.size() <= resPopSize) return toSelect;

        ArrayList<T> toSelectArray = new ArrayList<T>(toSelect.size());
        toSelectArray.addAll(toSelect);

        Integer[] toSelectLevel = new Integer[toSelect.size()];
        Arrays.fill(toSelectLevel, 0);

        Collection<Integer>[] v = new Collection[toSelect.size()];
        for (int p = 0; p < toSelectArray.size() ; p++) {
            v[p] = new LinkedList();
        }

        Collection<Integer> curFront = new LinkedList<Integer>();
        // fronts: F[i] = indexes of the individuals contained in the ith front
        // used to store the number of the first front
        int i = 0;

        for (int p = 0; p < toSelectArray.size() ; p++) {          
            for (int q = 0; q < toSelectArray.size() ; q++) {
                if (dominate.isDominated(toSelectArray.get(p), toSelectArray.get(q))) {
                 //   case LEFT:
                       toSelectLevel[p]++;
                       v[q].add(p);
                       i++;
          //              break;
           //         case RIGHT:
        //                toSelectLevel[q]++;
             //           v[p].add(q);
               //         break;
                }
            }

            // if no individual dominates p
            if (toSelectLevel[p] == 0) {
                curFront.add(p);
            }
        }
        // Logger.getLogger(FitnessByRank.class.getName()).info("Size: " + i);

       //System.out.println(Arrays.toString(toSelectLevel));

        Collection<T> ret = new LinkedList<T>();

        while(resPopSize >= ret.size() + curFront.size()) {
            Collection<Integer> nextFront = new LinkedList<Integer>();
             for(Integer p: curFront) {
                ret.add(toSelectArray.get(p));

                for(Integer q : v[p]) {
                    toSelectLevel[q]--;
                    if(toSelectLevel[q] == 0) {
                        nextFront.add(q);
                    }
                }
             }
             curFront = nextFront;
        }

        Collection<T> lastFront = new LinkedList<T>();

        for(Integer p: curFront) {
            lastFront.add(toSelectArray.get(p));
        }

        for(T point : crowding.orderByDecreasingCroding(lastFront, dim, operations)) {
            if(ret.size() >= resPopSize) break;
            ret.add(point);
        }

        return ret;
    }
}

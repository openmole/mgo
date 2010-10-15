/*
 * Copyright (C) 2010 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.tools.mgo.domination.diversity

import org.openmole.tools.mgo.model.MultiGoal
import org.openmole.tools.mgo.model.MultiGoal._
import org.openmole.tools.mgo.paretoquick.ParetoQuick
import scala.collection.immutable.TreeSet
import scala.collection.mutable.ArrayBuffer

object FitnessByRank {

    def selectByFitnessAndCrowdingPareto[T, MG <: MultiGoal[T]](toSelect: IndexedSeq[MG], resPopSize: Int)(implicit op: Integral[T]): IndexedSeq[MG] = {
        if(toSelect.isEmpty) toSelect
        else selectByFitnessAndCrowdingPareto[T,MG](toSelect, toSelect.iterator.next.goals.size, resPopSize)
    }

    def selectByFitnessAndCrowdingPareto[T, MG <: MultiGoal[T]](toSelect: IndexedSeq[MG], dim: Int, resPopSize: Int)(implicit op: Integral[T]): IndexedSeq[MG] =  {
      
        var all = new TreeSet[MG]
        all ++= toSelect

        val ret = new ArrayBuffer[MG](resPopSize)
        var pareto = ParetoQuick.pareto[T, MG](all, dim)
    
        while(resPopSize > ret.size + pareto.size) {
            ret ++= pareto
            all --= pareto
            pareto = ParetoQuick.pareto[T, MG](all, dim)
        }

        for(elt <- Crowding.orderByDecreasingCrowding[T, MG](pareto, dim)) {
            ret += elt
            if(ret.size >= resPopSize) return ret
        }
  
        ret
    }

    /*public <T extends IPoint> Collection<T> selectByFitnessAndCrowding(Collection<T> toSelect, int dim,  int resPopSize, Map<Class, IOperation> operations) {

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
    }*/
}

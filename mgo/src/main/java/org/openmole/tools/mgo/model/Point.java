/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.model;

/**
 *
 * @author salmamesmoudi
 */
public abstract class Point implements IPoint {

    @Override
    public int compareTo(IPoint o) {
        int compare;

        for(int i = 0; i < size(); i++) {
            compare = apply(i).compareTo(o.apply(i));
            if(compare != 0) return compare;
        }

        return 0;
    }



}

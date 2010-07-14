/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo.tools;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Cloner {
    static private com.rits.cloning.Cloner instance = new com.rits.cloning.Cloner();

    static public <T> T cloneObject(T obj) {
        return instance.deepClone(obj);
    }

}

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

package org.openmole.tools.mgo.model;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Operations {
    
    final public static Map<Class,IOperation> BaseOperations;

    static {

        Map<Class,IOperation> initBaseOperations = new HashMap<Class, IOperation>();


        IOperation<Byte> byteOperation = new IOperation<Byte>() {

            @Override
            public Byte substract(Byte first, Byte second) {
                return (byte)(first - second);
            }

            @Override
            public double divide(Byte first, Byte second) {
                return ((double) first) / second;
            }
        };

        initBaseOperations.put(Byte.TYPE, byteOperation);
        initBaseOperations.put(Byte.class, byteOperation);

        IOperation<Short> shortOperation = new IOperation<Short>() {

            @Override
            public Short substract(Short first, Short second) {
                return (short)(first - second);
            }

            @Override
            public double divide(Short first, Short second) {
                return ((double) first) / second;
            }
        };

        initBaseOperations.put(Short.TYPE, shortOperation);
        initBaseOperations.put(Short.class, shortOperation);

        IOperation<Integer> integerOperation = new IOperation<Integer>() {

            @Override
            public Integer substract(Integer first, Integer second) {
                return first - second;
            }

            @Override
            public double divide(Integer first, Integer second) {
                return ((double) first) / second;
            }
        };

        initBaseOperations.put(Integer.TYPE, integerOperation);
        initBaseOperations.put(Integer.class, integerOperation);

        IOperation<Long> longOperation = new IOperation<Long>() {

            @Override
            public Long substract(Long first, Long second) {
                return first - second;
            }

            @Override
            public double divide(Long first, Long second) {
                return ((double) first) / second;
            }
        };


        initBaseOperations.put(Long.TYPE, longOperation);
        initBaseOperations.put(Long.class, longOperation);

        IOperation<Float> floatOperation = new IOperation<Float>() {

            @Override
            public Float substract(Float first, Float second) {
                return first - second;
            }

            @Override
            public double divide(Float first, Float second) {
                 return ((double) first) / second;
            }
        };

        initBaseOperations.put(Float.TYPE, floatOperation);
        initBaseOperations.put(Float.class, floatOperation);

        IOperation<Double> doubleOperation = new IOperation<Double>() {

            @Override
            public Double substract(Double first, Double second) {
                return first - second;
            }

            @Override
            public double divide(Double first, Double second) {
                return ((double) first) / second;
            }
        };

        initBaseOperations.put(Double.TYPE, doubleOperation);
        initBaseOperations.put(Double.class, doubleOperation);

        initBaseOperations.put(BigInteger.class, new IOperation<BigInteger>() {

            @Override
            public BigInteger substract(BigInteger first, BigInteger second) {
                return first.subtract(second);
            }

            @Override
            public double divide(BigInteger first, BigInteger second) {
                return first.doubleValue() / second.doubleValue();
            }
        });

        initBaseOperations.put(BigDecimal.class, new IOperation<BigDecimal>() {

            @Override
            public BigDecimal substract(BigDecimal first, BigDecimal second) {
                return first.subtract(second);
            }

            @Override
            public double divide(BigDecimal first, BigDecimal second) {
                return first.divide(second).doubleValue();
            }
        });

        BaseOperations = Collections.unmodifiableMap(initBaseOperations);
    }
}

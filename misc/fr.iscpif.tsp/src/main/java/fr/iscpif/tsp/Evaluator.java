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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

/**
 *
 * @author Romain Reuillon <romain.reuillon at openmole.org>
 */
public class Evaluator {

    final private City[] citiesArray;
    final private double[][] distance;

    public Evaluator(File f) throws FileNotFoundException, IOException {

        List<City> cities = new LinkedList<City>();

        BufferedReader reader = null;

        reader = new BufferedReader(new FileReader(f));

        String line;

        while ((line = reader.readLine()) != null) {
            if (!line.matches(" *")) {
                Scanner scanner = new Scanner(line);
                scanner.useDelimiter(" ");

                scanner.next();
                double x = scanner.nextDouble();
                double y = scanner.nextDouble();

                cities.add(new City(x, y));
            }
        }

        reader.close();

        citiesArray = cities.toArray(new City[0]);
        distance = new double[citiesArray.length][citiesArray.length];

        for (int i = 0; i < citiesArray.length; i++) {
            for (int j = 0; j < citiesArray.length; j++) {
                City city1 = citiesArray[i];
                City city2 = citiesArray[j];
                distance[i][j] = Math.sqrt(Math.pow(city1.getX() - city2.getX(), 2) + Math.pow(city1.getY() - city2.getY(), 2));

            }
        }
    }

    public double evalTour(File f) throws FileNotFoundException, IOException {
        List<Integer> tour = new LinkedList<Integer>();
        BufferedReader reader = null;

        reader = new BufferedReader(new FileReader(f));

        String line;

        while ((line = reader.readLine()) != null) {
            if (!line.matches(" *")) {
                Scanner scanner = new Scanner(line);
                scanner.useDelimiter(" ");  
                tour.add(scanner.nextInt() - 1);
            }
        }

        reader.close();


        int[] toEval = new int[tour.size()];
        Iterator<Integer> it = tour.iterator();
        for(int i = 0 ; i < tour.size(); i++) {
            toEval[i] = it.next();
        }


        return evaluate(toEval);



    }

        public double evalTourRaw(File f) throws FileNotFoundException, IOException {
        List<Integer> tour = new LinkedList<Integer>();
        BufferedReader reader = null;

        reader = new BufferedReader(new FileReader(f));


        String line;

        while ((line = reader.readLine()) != null) {
            if (!line.matches(" *")) {
                Scanner scanner = new Scanner(line);
                scanner.useDelimiter(" ");
                tour.add(scanner.nextInt());
            }
        }

        reader.close();

        int[] toEval = new int[tour.size()];
        Iterator<Integer> it = tour.iterator();
        for(int i = 0 ; i < tour.size(); i++) {
            toEval[i] = it.next();
        }


        return evaluate(toEval);
   }



    public int[] toIndex(Genome genome) {
         class ToOrderComparable implements Comparable<ToOrderComparable> {
            final double value;
            final int position;

            public ToOrderComparable(double value, int position) {
                this.value = value;
                this.position = position;
            }

            @Override
            public int compareTo(ToOrderComparable o) {
                return Double.compare(value, o.value);
            }
        }

        float[] values = genome.getValues();
        ToOrderComparable[] ordered = new ToOrderComparable[values.length];

        for(int i = 0; i < values.length; i++) {
            ordered[i] = new ToOrderComparable(values[i], i);
        }

        Arrays.sort(ordered);

        int[] citiesGenome = new int[values.length];

        for(int i = 0; i < values.length; i++) {
            citiesGenome[ordered[i].position] = i;
        }

        return citiesGenome;
    }

    public double evaluate(Genome genome) {
        int[] citiesGenome = toIndex(genome);
        return evaluate(citiesGenome);
    }

     public double evaluate(int[] citiesGenome) {

        double totDistance = 0;
        int firstCity = citiesGenome[0];
        int curCity = firstCity;

        for(int i = 1; i < citiesGenome.length; i++) {
            int nextCity = citiesGenome[i];
            totDistance += distance[curCity][nextCity];
            curCity = nextCity;
        }

        totDistance += distance[curCity][firstCity];

        return totDistance;
     }

     public int size() {
         return citiesArray.length;
     }
}

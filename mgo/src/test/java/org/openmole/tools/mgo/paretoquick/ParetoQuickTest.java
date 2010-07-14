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
package org.openmole.tools.mgo.paretoquick;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import junit.framework.TestCase;
import org.openmole.tools.mgo.model.IPoint;


/**
 *
 * @author salmamesmoudi
 */
public class ParetoQuickTest extends TestCase {

    Random rd=new Random();
    public  Point generateur(int dim){

        Comparable[] coordonates= new Comparable[dim];
        for(int i=0;i<dim;i++){
            coordonates[i]=rd.nextInt(100);
        }

        Point p=new Point(coordonates);

        return p;
    }

    public  List<Double> generateurD(int dim){
        List<Double> coordinates = new ArrayList<Double>(dim);
        for(int i=0;i<dim;i++){
            coordinates.add(rd.nextDouble());
        }
        return coordinates;
    }


    public void testPareto() {
        ParetoQuick pareto = new ParetoQuick();

        List<Point> vectPoint = new ArrayList<Point> (10);
        // vectPoint.add(new Point( -1, 2., 44));
        vectPoint.add(new Point( -1, 2., 44));
        vectPoint.add(new Point( 2, 3.4, 4));
        vectPoint.add(new Point( 1, 7., 9));
        vectPoint.add(new Point( 0, 2., 0));
        vectPoint.add(new Point( 3, 0., 9));
        vectPoint.add(new Point( 4, 44., 0));
        vectPoint.add(new Point( 1, 7., 9));
        vectPoint.add(new Point( 3, 2.8, 1));
        vectPoint.add(new Point( 3, 2., 9));
        vectPoint.add(new Point( 90, 3.4, 4));
        vectPoint.add(new Point( -2, 7., 9));
        vectPoint.add(new Point( 1, 2., 44));
        vectPoint.add(new Point( 3, 0., 9));
        

        assertEquals(pareto.pareto(vectPoint, 3).size(),11);

        vectPoint = new ArrayList<Point> (40);
        vectPoint.add(new Point(0.96,6,4));
        vectPoint.add(new Point(0.84000003,8,4));
vectPoint.add(new Point(0.96,8,3));
vectPoint.add(new Point(0.84000003,9,3));
vectPoint.add(new Point(0.96,9,3));
vectPoint.add(new Point(0.84000003,10,3));
vectPoint.add(new Point(0.75,11,2));
vectPoint.add(new Point(0.75,11,3));
vectPoint.add(new Point(0.75,11,4));
vectPoint.add(new Point(0.75,12,2));
vectPoint.add(new Point(0.75,12,3));
vectPoint.add(new Point(0.75,13,3));
vectPoint.add(new Point(0.75,14,3));
vectPoint.add(new Point(0.84000003,21,3));
vectPoint.add(new Point(0.75,22,3));
vectPoint.add(new Point(0.75,22,4));
vectPoint.add(new Point(0.84000003,22,2));
vectPoint.add(new Point(0.84000003,22,3));
vectPoint.add(new Point(0.96,22,3));
vectPoint.add(new Point(0.8,25,4));
vectPoint.add(new Point(0.96,6,3));
vectPoint.add(new Point(0.84000003,8,2));
vectPoint.add(new Point(0.84000003,8,3));
vectPoint.add(new Point(0.84000003,8,4));


assertEquals(pareto.pareto(vectPoint, 3).size(),19);

/*for(IPoint p: vectPoint)
 System.out.println(p);


        for(IPoint p: pareto.pareto(vectPoint, 3))
            System.out.println(p);*/


       /* System.out.println("calcul de la diversite");
        SharingDiversity shDiversity = new SharingDiversity();

       Vector<Double> V= shDiversity.setSimilarities(vectPoint);

       for(double v : V) {
            System.out.println(v);
        }
*/
//        Vector <Point> vectPoint = new Vector <Point> (10);
//        for(int i=0;i<10;i++){
//            vectPoint.add(generateur(5));
//            }
//
//        for(Point point : vectPoint) {
//            System.out.println(point.toString());
//        }


//        System.out.println("vecteur range");
//        OrderPointOneDim<Point> opod=new OrderPointOneDim<Point>(1);
//        opod.addAll(vectPoint);
//
//        Vector<Point> v = new Vector<Point>(vectPoint.size());
//
//        for (Point pp : opod) {
//            v.add(pp);
//        }
//
//       for(Point point : v) {
//            System.out.println(point.toString());
//       }
//
//        System.out.println("--------------");
//
//        Pareto2D ParetoD = new Pareto2D();
//
//        Vector < Point > archive=ParetoD.archivePreto2D(v);
//System.out.println("archive");
////        vectPoint.add(P1);
////        vectPoint.add(P2);
////        vectPoint.add(P3);
////        vectPoint.add(P4);
//        OrderPointOneDim opod=new OrderPointOneDim(4);
//
//        opod.addAll(vectPoint);
//        Marry Mariage =new Marry();
//        IDominate dominate=new DominateMinimumStrict();
////        Vector < Point > archive=p.pareto(vectPoint, 3, dominate);
//        Vector < Point > archive=Mariage.eliminer(vectPoint, 4, dominate);
//
//       for(Point point : archive) {
//            System.out.println(point.toString());
//        }

//        for(Point point : opod) {
//            System.out.println(point.toString());
//        }
//
        //IDominate d= new DominateMinimumStrict ();
//
       // System.out.println(d.dominated(P3,P4));
//
//        Vector <Point> vect = new Vector <Point> (3);
//
//        Convert C= new Convert(opod);
//        vect=C.convertVect();
//
//        //Vector <Point> archive = new Vector <Point> ();
//        Pareto2D P=new Pareto2D();
//        archive=P.archivePreto2D(vect);
//
//        for(Point point : archive) {
//            System.out.println(point.toString());
//        }
        

    }
}

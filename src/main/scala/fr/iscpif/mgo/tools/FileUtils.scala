/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.mgo.tools
import java.io._
import java.util.StringTokenizer
import fr.iscpif.mgo.Individual
import fr.iscpif.mgo.ga.GAFitness
import fr.iscpif.mgo.ga.GAGenome
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.io.Source
import Individual._

object FileUtils {

  def readFront(path:String):Array[Array[Double]]={
 
  val list = new ArrayBuffer[Array[Double]]()
    try {

      Source.fromFile(path).getLines().foreach{ line =>
        list += line.split("[ \t]+").flatMap { s =>
          try {
            List(s.toDouble)
          } catch {
            case _ => Nil
          }
        }
      }
  
    } catch {
      case ioe: IOException =>  println("Error, mgo crashed reading for file: "+path);
      case e: Exception => println("Exception on file "+path);
    }
    list.toArray
  }

  /**
   * Used for reading/writing to database, files, etc.
   * Code From the book "Beginning Scala"
   * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
   */
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
  
  def writeToFile(file:File, data:String) = 
  using (new FileWriter(file)) {
    fileWriter => fileWriter.write(data)
  }

   def convertMatrixFrontToIndividuals(path:String)={
    
    val matrixFront = readFront(path)
    
    val individualFront= matrixFront.map{
      i =>  new Individual[GAGenome, GAFitness] {
        def genome = null //genome n'existe pas dans ce cas la ...
        def fitness = new GAFitness {
          val fitness = i.toIndexedSeq
        }
      }
    }
  }
  
  
}


/**
 * This method reads a Pareto Front for a file.
 * @param path The path to the file that contains the pareto front
 * @return double [][] whit the pareto front
 **/
/*  public double [][] readFront(String path) {
 try {
 // Open the file
 FileInputStream fis   = new FileInputStream(path)     ;
 InputStreamReader isr = new InputStreamReader(fis)    ;
 BufferedReader br      = new BufferedReader(isr)      ;
      
 List<double []> list = new ArrayList<double []>();
 int numberOfObjectives = 0;
 String aux = br.readLine();
 while (aux!= null) {
 StringTokenizer st = new StringTokenizer(aux);
 int i = 0;
 numberOfObjectives = st.countTokens();
 double [] vector = new double[st.countTokens()];
 while (st.hasMoreTokens()) {
 double value = (new Double(st.nextToken())).doubleValue();
 vector[i] = value;
 i++;
 }
 list.add(vector);
 aux = br.readLine();
 }
            
 br.close();
      
 double [][] front = new double[list.size()][numberOfObjectives];
 for (int i = 0; i < list.size(); i++) {
 front[i] = list.get(i);
 }
 return front;
      
 } catch (Exception e) {
 System.out.println("InputFacilities crashed reading for file: "+path);
 e.printStackTrace();
 }
 return null;
 } // readFront
 */
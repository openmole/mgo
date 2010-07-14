package fr.iscpif.tsp;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.openmole.tools.distrng.prng.IPRNG;
import org.openmole.tools.distrng.prng.SecureRandomRNG;
import org.openmole.tools.distrng.prng.WELL1024;
import org.openmole.tools.distrng.prng.parallelization.IndexSequence;
import org.openmole.tools.mgo.type.IGenomes;

/**
 * Hello world!
 *
 */
public class App {

    public static void main(String[] args) {
        try {

            Logger.getLogger(App.class.getName()).addHandler(new FileHandler("/iscpif/users/reuillon/work/TSP/log" + System.currentTimeMillis() + ".log"));
            File f = new File("/iscpif/users/reuillon/work/TSP", "att48.tsp");

            int size = 1000;
            int nbMutationDisplay = 10000000;

            Evaluator evaluator = new Evaluator(f);

            File f2 = new File("/iscpif/users/reuillon/work/TSP", "att48.opt");
            double optim = evaluator.evalTour(f2);

/*            File f3 = new File("/iscpif/users/reuillon/work/TSP", "found.opt");
                    double optimfound = evaluator.evalTourRaw(f3);

                    System.out.println(optim + " " + optimfound);
*/
            EvolutionEngine evolutionEngine = new EvolutionEngine();
            evolutionEngine.addOperation(new SegmentMutation(), 7);
            evolutionEngine.addOperation(new AverageCrossover(), 1);
            evolutionEngine.addOperation(new UniformeCrossOver(), 1);
            evolutionEngine.addOperation(new RandomMutation(), 1);

            SecureRandomRNG secureRandomRNG = new SecureRandomRNG();
            IndexSequence<WELL1024> indexSequence = new IndexSequence<WELL1024>(secureRandomRNG, WELL1024.class);
            IPRNG prng = indexSequence.iterator().next();
            
            IGenomes<Genome> genomes = GenomeFactory.getInstance().randomGenomes(evaluator.size(), size, prng);

            Individu[] population = new Individu[genomes.size()];

            int i = 0;
            for (Genome g : genomes) {
                double distance = evaluator.evaluate(g);
                population[i++] = new Individu(g, distance);
            }

            genomes = null;

            int curDisp = nbMutationDisplay;

            while (true) {
                Genome newGenome = evolutionEngine.makeEvolve(population, prng);
                double distance = evaluator.evaluate(newGenome);

                int indice = prng.nextInt(population.length);
                Individu rndmIndiv = population[indice];

                if (distance < rndmIndiv.getDistance()) {
                    population[indice] = new Individu(newGenome, distance);
                }

                if (curDisp-- == 0) {
                    curDisp = nbMutationDisplay;
                    int optimumIndiv = 0;

                    for (int j = 1; j < population.length; j++) {
                        if (population[j].getDistance() < population[optimumIndiv].getDistance()) {
                            optimumIndiv = j;
                        }
                    }

                    Logger.getLogger(App.class.getName()).info("optim = " + optim + " ; " + population[optimumIndiv].getDistance()+ " ; "+Arrays.toString(evaluator.toIndex(population[optimumIndiv].getGenome())));
                }

                /*else {
                nbgenNoChange++;
                }*/

                /* if(maxNbgenNoChange < nbgenNoChange) {
                maxNbgenNoChange = nbgenNoChange;
                System.out.println(maxNbgenNoChange);
                }*/
            }





        } catch (FileNotFoundException ex) {
            Logger.getLogger(App.class.getName()).log(Level.SEVERE, "", ex);
        } catch (IOException ex) {
            Logger.getLogger(App.class.getName()).log(Level.SEVERE, "", ex);
        }



    }
}

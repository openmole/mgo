MGO
===

MGO is a library based on the cake pattern for multi-objective evolutionary algorithm:
* written in scala,
* enforcing immutability,
* exposes a modular and exetensible architechture,
* implements state of the art algorithms.

Documentation
-------------

The scaladoc of mgo is available here: [scaladoc](http://romainreuillon.github.com/mgo/scaladoc).

Licence
-------

MGO is licenced under the GNU Affero GPLv3 software licence. 

Example
-------

    import java.util.Random 
    import fr.iscpif.mgo._

    val zdt = new ZDT4 {
      def n = 10
    }
  
    implicit val rng = new Random
  
    val smsemoea =
      new SMSEMOEASigma
        with BinaryTournamentSelection
        with HyperVolumeStabilityTermination
        with NonDominatedElitism
        with CoEvolvingSigmaValuesMutation
        with SBXBoundedCrossover
        with HypervolumeDiversity
        with ParetoRanking
        with StrictDominance
        with RankDiversityModifier {
        def distributionIndex = 2
        
        def windowSize = 100
        def deviationEpsilon = 0.001
        def mu = 200
        def lambda = 200
        def genomeSize = 10
        def referencePoint = IndexedSeq(2.0, 2.0)
      }
  
    val res = smsemoea.run(zdt).dropWhile {
      s => println(s.terminationState.std); !s.terminated
    }.next.population
    res sortBy (_.metaFitness.rank) foreach {
      e => println(zdt.scale(e)._2.values.mkString(","))
    }
  
Maven dependency
----------------

    <dependency>
      <groupId>fr.iscpif</groupId>
      <artifactId>mgo</artifactId>
      <version>1.XX</version>
    </dependency>
    
    <repository>
      <id>maven.iscpif.fr</id>
      <name>ISC-PIF Repository</name>
      <url>http://maven.iscpif.fr/public/</url>
    </repository>


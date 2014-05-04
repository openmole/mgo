MGO
===

MGO is a library based on the cake pattern for multi-objective evolutionary algorithm:
* written in scala,
* enforcing immutability,
* exposes a modular and exetensible architechture,
* implements state of the art algorithms,
* take advantage of multi-core architectures.

Licence
-------

MGO is licenced under the GNU Affero GPLv3 software licence. 

Example
-------

Have a look at the test directory in the repository.
  
SBT dependency
----------------

    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

    libraryDependencies += "fr.iscpif" %% "mgo" % "1.72-SNAPSHOT"


organization := "fr.iscpif"

name := "mgo"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "ISC-PIF Release" at "http://maven.iscpif.fr/release",
  "ISC-PIF Snapshots" at "http://maven.iscpif.fr/snapshots",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

publishMavenStyle := true

publishArtifact in Test := false

//publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

publishTo <<= version { (v: String) =>
  val maven = "http://maven.iscpif.fr/"
  if (v.trim.endsWith("SNAPSHOT")) 
   Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))) 
   //Some("snapshots" at maven + "snapshots") 
  else
    Some("releases"  at maven + "releases")
}

pomExtra := (
  <url>https://github.com/romainreuillon/mgo</url>
  <licenses>
    <license>
      <name>Affero GPLv3</name>
      <url>http://www.gnu.org/licenses/</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:romainreuillon/mgo.git</url>
    <connection>scm:git:git@github.com:romainreuillon/mgo.git</connection>
  </scm>
  <developers>
    <developer>
      <id>romainreuillon</id>
      <name>Romain Reuillon</name>
    </developer>
  </developers>
)


credentials += Credentials(Path.userHome / ".sbt" / "iscpif.credentials")

scalariformSettings

releaseSettings

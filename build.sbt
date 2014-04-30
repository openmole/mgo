organization := "fr.iscpif"

name := "mgo"

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.10.4", "2.11.0")

resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

publishMavenStyle := true

publishArtifact in Test := false

publishTo <<= version { (v: String) =>
  val maven = "http://maven.iscpif.fr/"
  if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at maven + "snapshots") 
  else Some("releases"  at maven + "releases")
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

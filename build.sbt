organization := "fr.iscpif"

name := "mgo"

scalaVersion := "2.11.7"

//scalacOptions in (Compile,doc) ++= Seq("-diagrams")  //Seq("-groups", /*"-implicits",*/ "-diagrams","-diagrams-max-classes","20")
//scalacOptions ++= Seq("-feature")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full)

resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

val monocleVersion = "1.0.1"

libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion

libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion

libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion

val scalazVersion = "7.1.5"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

homepage := Some(url("https://github.com/openmole/mgo"))

scmInfo := Some(ScmInfo(url("https://github.com/openmole/mgo.git"), "scm:git:git@github.com:openmole/mgo.git"))

pomExtra := (
  <developers>
    <developer>
      <id>romainreuillon</id>
      <name>Romain Reuillon</name>
    </developer>
    <developer>
      <id>guillaumecherel</id>
      <name>Guillaume Cherel</name>
    </developer>
  </developers>
)

scalariformSettings

releaseSettings

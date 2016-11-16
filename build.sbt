organization := "fr.iscpif"

name := "mgo"


scalaOrganization := "org.typelevel"
scalaVersion := "2.12.0"
crossScalaVersions := Seq("2.11.8", "2.12.0")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
scalacOptions ++= Seq("-Ypartial-unification")

val monocleVersion = "1.3.2"

//libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.15.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6"
libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion
libraryDependencies +=  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
//libraryDependencies += "com.projectseptember" %% "freek" % "0.6.0"

val scalazVersion = "7.2.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)


resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"


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

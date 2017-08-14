organization := "fr.iscpif"
name := "mgo"

scalaVersion := "2.12.2"
crossScalaVersions := Seq("2.11.11", "2.12.2")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("staging")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")

// macro paradise doesn't work with scaladoc
sources in (Compile, doc) := Nil

val monocleVersion = "1.4.0"
val freedslVersion = "0.12"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion
libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion
libraryDependencies += "fr.iscpif.freedsl" %% "dsl" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "random" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "io" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "tool" % freedslVersion

libraryDependencies += "org.typelevel"  %% "squants"  % "1.3.0"

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
      <name>Guillaume Chérel</name>
    </developer>
  </developers>
)

scalariformSettings

releaseSettings

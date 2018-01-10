organization := "fr.iscpif"
name := "mgo"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.12.4")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
scalacOptions += "-Xplugin-require:macroparadise"

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("staging")
resolvers += Resolver.sonatypeRepo("snapshots")

// macro paradise doesn't work with scaladoc
sources in (Compile, doc) := Nil

val monocleVersion = "1.4.0"
val freedslVersion = "0.23"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion
libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion

libraryDependencies += "fr.iscpif.freedsl" %% "random" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "io" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "tool" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "dsl" % freedslVersion
libraryDependencies += "fr.iscpif.freedsl" %% "system" % freedslVersion

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

releasePublishArtifactsAction := PgpKeys.publishSigned.value

releaseVersionBump := sbtrelease.Version.Bump.Minor

releaseTagComment    := s"Releasing ${(version in ThisBuild).value}"

releaseCommitMessage := s"Bump version to ${(version in ThisBuild).value}"

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

scalariformSettings(true)


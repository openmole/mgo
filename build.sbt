
name := "mgo"
ThisBuild / organization := "org.openmole"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / crossScalaVersions := Seq("2.13.6")

val monocleVersion = "3.0.0"

def scala2(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, _))  => true
    case _             => false
  }

lazy val settings: Seq[Setting[_]] = Seq(
  //addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("staging"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-target:jvm-1.8"),
  scalariformAutoformat := true,
  scalacOptions ++= Seq("-language:postfixOps")
//  scalacOptions ++= (
//    if (priorTo2_13(scalaVersion.value)) Nil else Seq("-Ymacro-annotations", "-language:postfixOps")
//  ),
//  libraryDependencies ++=
//    (if (priorTo2_13(scalaVersion.value))
//      Seq(
//        compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
//      )
//    else Nil)
  //libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6"
) ++ scalariformSettings(true)

lazy val mgo = Project(id = "mgo", base = file("mgo")) settings(settings: _*) settings (
  // macro paradise doesn't work with scaladoc
  //Compile / sources in (Compile, doc) := Nil,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",

  libraryDependencies += "dev.optics"  %%  "monocle-core"    % monocleVersion,
  libraryDependencies += "dev.optics" %% "monocle-macro" % "3.0.0-RC2",
//  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
//  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,

  libraryDependencies += "org.typelevel"  %% "squants"  % "1.6.0" cross(CrossVersion.for3Use2_13),

//  libraryDependencies ++= (if(scala2(scalaVersion.value)) Seq("org.typelevel"  %% "squants"  % "1.6.0") else Seq()),

  //libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1" cross(CrossVersion.for3Use2_13),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
)


/* Publish */

ThisBuild / publishMavenStyle := true
ThisBuild / Test / publishArtifact := false
publishArtifact := false
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := sonatypePublishToBundle.value

ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

ThisBuild / homepage := Some(url("https://github.com/openmole/mgo"))

ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/openmole/mgo.git"), "scm:git:git@github.com:openmole/mgo.git"))

ThisBuild / pomExtra := (
  <developers>
    <developer>
      <id>romainreuillon</id>
      <name>Romain Reuillon</name>
    </developer>
    <developer>
      <id>guillaumecherel</id>
      <name>Guillaume Chérel</name>
    </developer>
    <developer>
      <id>justeraimbault</id>
      <name>Juste Raimbault</name>
    </developer>
  </developers>
)

releasePublishArtifactsAction := PgpKeys.publishSigned.value

releaseVersionBump := sbtrelease.Version.Bump.Minor

releaseTagComment := s"Releasing ${(ThisBuild / version).value}"

releaseCommitMessage := s"Bump version to ${(ThisBuild / version).value}"

sonatypeProfileName := "org.openmole"

publishConfiguration := publishConfiguration.value.withOverwrite(true)

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  //releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)



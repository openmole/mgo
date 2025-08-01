
name := "mgo"
ThisBuild / organization := "org.openmole"
ThisBuild / scalaVersion := "3.7.1"

val monocleVersion = "3.2.0"

lazy val settings: Seq[Setting[_]] = Seq(
  //addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("staging"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  scalacOptions ++= Seq("-Xtarget:11", "-language:higherKinds"),
  scalacOptions ++= Seq("-language:postfixOps", "-source:3.7")
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
)

lazy val mgo = Project(id = "mgo", base = file("mgo")) settings(settings: _*) settings (
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "dev.optics"  %%  "monocle-core" % monocleVersion,
  libraryDependencies += "dev.optics" %% "monocle-macro" % monocleVersion,
  libraryDependencies += "org.typelevel"  %% "squants"  % "1.8.3", //cross(CrossVersion.for2_13Use3),
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  excludeDependencies += ExclusionRule(organization = "org.typelevel", name = "cats-kernel_2.13"),
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
)


/* Publish */

ThisBuild / publishMavenStyle := true
ThisBuild / Test / publishArtifact := false
publishArtifact := false

ThisBuild / licenses := Seq("GPLv3" -> url("http://www.gnu.org/licenses/"))
ThisBuild / homepage := Some(url("https://github.com/openmole/mgo"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/openmole/mgo.git"), "scm:git:git@github.com:openmole/mgo.git"))

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

ThisBuild / developers := List(
  Developer(
    id    = "romainreuillon",
    name  = "Romain Reuillon",
    email = "",
    url   = url("https://github.com/romainreuillon/")
  )
)

releasePublishArtifactsAction := PgpKeys.publishSigned.value
releaseVersionBump := sbtrelease.Version.Bump.Minor
releaseTagComment := s"Releasing ${(ThisBuild / version).value}"
releaseCommitMessage := s"Bump version to ${(ThisBuild / version).value}"
publishConfiguration := publishConfiguration.value.withOverwrite(true)

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("publishSigned"),
  releaseStepCommand("sonaRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)



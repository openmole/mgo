
name := "mgo"
ThisBuild / organization := "org.openmole"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / crossScalaVersions := Seq("3.3.3")

val monocleVersion = "3.2.0"

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
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  scalacOptions ++= Seq("-Xtarget:11", "-language:higherKinds"),
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
)

lazy val mgo = Project(id = "mgo", base = file("mgo")) settings(settings: _*) settings (
  // macro paradise doesn't work with scaladoc
  //Compile / sources in (Compile, doc) := Nil,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",

  libraryDependencies += "dev.optics"  %%  "monocle-core" % monocleVersion,
  libraryDependencies += "dev.optics" %% "monocle-macro" % monocleVersion,
//  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
//  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,

  libraryDependencies += "org.typelevel"  %% "squants"  % "1.8.3", //cross(CrossVersion.for2_13Use3),

//  libraryDependencies ++= (if(scala2(scalaVersion.value)) Seq("org.typelevel"  %% "squants"  % "1.6.0") else Seq()),

  //libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.2",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % "2.1.0"
    //"org.scalanlp" %% "breeze-natives" % breezeVersion
  ),
  excludeDependencies += ExclusionRule(organization = "org.typelevel", name = "cats-kernel_2.13"),
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
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



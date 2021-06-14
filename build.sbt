
name := "mgo"
organization in ThisBuild := "org.openmole"
scalaVersion in ThisBuild := "2.13.6"
crossScalaVersions in ThisBuild := Seq("2.12.12", "2.13.6")

val monocleVersion = "2.0.3"

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val settings: Seq[Setting[_]] = Seq(
  //addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("staging"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq("-target:jvm-1.8"),
  scalariformAutoformat := true,
  scalacOptions ++= (
    if (priorTo2_13(scalaVersion.value)) Nil else Seq("-Ymacro-annotations", "-language:postfixOps")
  ),
  libraryDependencies ++=
    (if (priorTo2_13(scalaVersion.value))
      Seq(
        compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
      )
    else Nil),
  libraryDependencies += "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6"
) ++ scalariformSettings(true)

lazy val mgo = Project(id = "mgo", base = file("mgo")) settings(settings: _*) settings (
  // macro paradise doesn't work with scaladoc
  sources in (Compile, doc) := Nil,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",

  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,

  libraryDependencies += "org.typelevel"  %% "squants"  % "1.6.0",

  //libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0",
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
)


/* Publish */

publishMavenStyle in ThisBuild := true
publishArtifact in Test in ThisBuild := false
publishArtifact := false
pomIncludeRepository in ThisBuild := { _ => false }

publishTo in ThisBuild := sonatypePublishToBundle.value

pomIncludeRepository in ThisBuild := { _ => false }

licenses in ThisBuild := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

homepage in ThisBuild := Some(url("https://github.com/openmole/mgo"))

scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/openmole/mgo.git"), "scm:git:git@github.com:openmole/mgo.git"))

pomExtra in ThisBuild := (
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

releaseTagComment := s"Releasing ${(version in ThisBuild).value}"

releaseCommitMessage := s"Bump version to ${(version in ThisBuild).value}"

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



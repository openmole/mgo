

organization in ThisBuild := "fr.iscpif"
scalaVersion in ThisBuild := "2.12.8"
crossScalaVersions in ThisBuild := Seq("2.12.8")

val monocleVersion = "1.6.0"

def settings = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  resolvers += Resolver.sonatypeRepo("public"),
  resolvers += Resolver.sonatypeRepo("staging"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  scalacOptions ++= Seq("-target:jvm-1.8"),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalariformAutoformat := true
) ++ scalariformSettings(true)

lazy val mgo = Project(id = "mgo", base = file("mgo")) settings(settings: _*) settings (

  // macro paradise doesn't work with scaladoc
  sources in (Compile, doc) := Nil,
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",

  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,

  libraryDependencies += "org.typelevel"  %% "squants"  % "1.4.0",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0",
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
) dependsOn(tagtools) 


lazy val tagtools = Project(id = "tagtools", base = file("tagtools")) settings(settings: _*) settings (
  libraryDependencies += "io.frees" %% "frees-core" % "0.8.2",
)



/* Publish */

publishMavenStyle in ThisBuild := true
publishArtifact in Test in ThisBuild := false
publishArtifact := false
pomIncludeRepository in ThisBuild := { _ => false }

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

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
  </developers>
)

releasePublishArtifactsAction := PgpKeys.publishSigned.value

releaseVersionBump := sbtrelease.Version.Bump.Minor

releaseTagComment := s"Releasing ${(version in ThisBuild).value}"

releaseCommitMessage := s"Bump version to ${(version in ThisBuild).value}"

sonatypeProfileName := "fr.iscpif"

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



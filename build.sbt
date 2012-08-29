organization := "fr.iscpif"

name := "mgo"

version := "1.33"

scalaVersion := "2.9.2"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)


seq(netbeans.NetbeansTasks.netbeansSettings:_*)

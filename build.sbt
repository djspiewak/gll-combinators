name := "gll-combinators"

organization := "edu.uwm.cs"

version := "1.5-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources,
  "org.scala-tools.testing" %% "specs" % "1.6.9" % "test" withSources)
  
publishArtifact in (Compile, packageDoc) := false

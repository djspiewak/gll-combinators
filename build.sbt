name := "gll-combinators"

organization := "edu.uwm.cs"

version := "1.5-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources,
  "org.scala-tools.testing" %% "specs" % "1.6.9" % "test" withSources)
  
publishArtifact in (Compile, packageDoc) := false

credentials += Credentials(Path.userHome / ".ivy2" / ".rgcredentials")

publishTo := Some("ReportGrid Nexus" at "http://devci01.reportgrid.com:8081/content/repositories/snapshots")

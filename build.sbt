name := "gll-combinators"

organization := "edu.uwm.cs"

version := "1.5-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources,
  "org.scala-tools.testing" %% "specs" % "1.6.9" % "test" withSources)
  
publishArtifact in (Compile, packageDoc) := false

credentials += Credentials(Path.userHome / ".ivy2" / ".rgcredentials")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.reportgrid.com/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"snapshots/") 
  else                                   Some("releases"  at nexus+"releases/")
},
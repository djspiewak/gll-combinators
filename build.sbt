name := "gll-combinators"

organization := "com.codecommit"

version := "2.2-SNAPSHOT"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("https://github.com/djspiewak/gll-combinators"))

scalaVersion := "2.11.0-M8"

crossScalaVersions := Seq("2.10.3", "2.11.0-M8")

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
      "org.specs2" %% "specs2"     % "2.3.7"  % "test"
)

logBuffered := false       // gives us incremental output from Specs2
  
publishArtifact in (Compile, packageDoc) := false

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <scm>
    <url>git://github.com/djspiewak/gll-combinators.git</url>
    <connection>scm:git:git://github.com/djspiewak/gll-combinators.git</connection>
  </scm>
  <developers>
    <developer>
      <id>djspiewak</id>
      <name>Daniel Spiewak</name>
      <url>http://www.codecommit.com/blog</url>
    </developer>
  </developers>)

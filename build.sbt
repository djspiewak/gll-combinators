name := "gll-combinators"

organization := "com.codecommit"

version := "2.1-SNAPSHOT"

licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("https://github.com/djspiewak/gll-combinators"))

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test" withSources,
  "org.scala-tools.testing" %% "specs" % "1.6.9" % "test" withSources)
  
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
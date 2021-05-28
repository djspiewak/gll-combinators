name := "gll-combinators"
organization := "com.codecommit"

baseVersion := "2.5"
strictSemVer := false

ThisBuild / publishFullName := "Daniel Spiewak"
ThisBuild / publishGithubUser := "djspiewak"

ThisBuild / crossScalaVersions := Seq("2.13.5", "2.12.14")
ThisBuild / scalaVersion := "2.13.5"

licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("https://github.com/djspiewak/gll-combinators"))

scmInfo := Some(ScmInfo(url("https://github.com/djspiewak/gll-combinators"),
  "git@github.com:djspiewak/gll-combinators.git"))

Test / parallelExecution := false

val Specs2Version = "4.11.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-collection-compat" % "2.4.3",
  "org.scalacheck"         %% "scalacheck"              % "1.15.4" % "test",

  "org.specs2"             %% "specs2-core"             % Specs2Version  % "test",
  "org.specs2"             %% "specs2-scalacheck"       % Specs2Version  % "test")

unmanagedSourceDirectories in Test += baseDirectory.value / "examples" / "src"
unmanagedResourceDirectories in Test += baseDirectory.value / "examples" / "input"

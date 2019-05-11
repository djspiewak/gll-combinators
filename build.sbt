name := "gll-combinators"
organization := "com.codecommit"

baseVersion := "2.4"
strictSemVer := false

ThisBuild / publishFullName := "Daniel Spiewak"
ThisBuild / publishGithubUser := "djspiewak"

licenses := Seq("BSD-3-Clause" -> url("http://www.opensource.org/licenses/bsd-license.php"))

homepage := Some(url("https://github.com/djspiewak/gll-combinators"))

scmInfo := Some(ScmInfo(url("https://github.com/djspiewak/gll-combinators"),
  "git@github.com:djspiewak/gll-combinators.git"))

Test / parallelExecution := false

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"        % "1.13.4" % "test",

  "org.specs2"     %% "specs2-core"       % "3.8.6"  % "test",
  "org.specs2"     %% "specs2-scalacheck" % "3.8.6"  % "test")

unmanagedSourceDirectories in Test += baseDirectory.value / "examples" / "src"
unmanagedResourceDirectories in Test += baseDirectory.value / "examples" / "input"

import Dependencies._

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.algebraicjulia"
ThisBuild / organizationName := "AlgebraicJulia"

lazy val root = (project in file("."))
  .settings(
    name := "semagrams",
    libraryDependencies += scalaTest % Test,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.2.0",
    libraryDependencies += "com.raquo" %%% "laminar" % "0.13.1",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.9" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

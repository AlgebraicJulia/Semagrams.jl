ThisBuild / scalaVersion := "3.0.1"
ThisBuild / crossScalaVersions ++= Seq("2.13.6", "3.0.1")
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.algebraicjulia"
ThisBuild / organizationName := "AlgebraicJulia"

lazy val root = (project in file("."))
  .settings(
    name := "semagrams",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      ("org.scala-js" %%% "scalajs-dom" % "1.2.0")
        .cross(CrossVersion.for3Use2_13),
      "com.raquo" %%% "laminar" % "0.13.1",
      "org.typelevel" %%% "cats-core" % "2.6.1",
      "org.typelevel" %%% "cats-kernel" % "2.6.1",
      "org.typelevel" %%% "cats-parse" % "0.3.4",
      "dev.optics" %%% "monocle-core" % "3.0.0",
      "dev.optics" %%% "monocle-macro" % "3.0.0",
      "com.lihaoyi" %%% "utest" % "0.7.10" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.15.4" % "test",
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin)

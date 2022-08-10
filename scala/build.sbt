ThisBuild / scalaVersion := "3.1.0"
ThisBuild / crossScalaVersions ++= Seq("2.13.6", "3.1.0")
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.algebraicjulia"
ThisBuild / organizationName := "AlgebraicJulia"
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "2.1.0",
  "com.raquo" %%% "laminar" % "0.14.2",
  "io.laminext" %%% "websocket" % "0.14.3",
  "io.laminext" %%% "websocket-upickle" % "0.14.3",
  "com.lihaoyi" %%% "upickle" % "1.4.3",
  "org.typelevel" %%% "cats-core" % "2.7.0",
  "org.typelevel" %%% "cats-kernel" % "2.7.0",
  "org.typelevel" %%% "cats-parse" % "0.3.4",
  "org.typelevel" %%% "cats-effect" % "3.3.5",
  "dev.optics" %%% "monocle-core" % "3.1.0",
  "dev.optics" %%% "monocle-macro" % "3.1.0",
  "com.lihaoyi" %%% "utest" % "0.7.10" % "test",
  "org.scalacheck" %%% "scalacheck" % "1.15.4" % "test",
)

lazy val core = (project in file("."))
  .settings(
    name := "semagrams",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin)

lazy val graph_app = (project in file("apps/graph"))
  .settings(
    name := "semagrams-graph-app",
    scalaJSUseMainModuleInitializer := true,
  )
  .dependsOn(core)
  .enablePlugins(ScalaJSPlugin)

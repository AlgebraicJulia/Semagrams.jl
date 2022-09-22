import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.1.3"
ThisBuild / crossScalaVersions ++= Seq("2.13.6", "3.1.3")
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.algebraicjulia"
ThisBuild / organizationName := "AlgebraicJulia"
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "2.2.0",
  "com.raquo" %%% "laminar" % "0.14.2",
  "io.laminext" %%% "websocket" % "0.14.3",
  "io.laminext" %%% "websocket-upickle" % "0.14.3",
  "com.lihaoyi" %%% "upickle" % "2.0.0",
  "org.typelevel" %%% "cats-core" % "2.8.0",
  "org.typelevel" %%% "cats-kernel" % "2.8.0",
  "org.typelevel" %%% "cats-effect" % "3.3.14",
  "dev.optics" %%% "monocle-core" % "3.1.0",
  "dev.optics" %%% "monocle-macro" % "3.1.0",
  "com.lihaoyi" %%% "utest" % "0.8.0" % "test",
)

ThisBuild / scalacOptions ++= Seq(
  "-Ykind-projector:underscores"
)

lazy val core = (project in file("."))
  .settings(
    name := "semagrams",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("semagrams"))
        )
    },
  )
  .enablePlugins(ScalaJSPlugin)

// lazy val graph_app = (project in file("apps/graph"))
//   .settings(
//     name := "semagrams-graph-app",
//     // scalaJSUseMainModuleInitializer := true,
//     scalaJSLinkerConfig ~= {
//       _.withModuleKind(ModuleKind.ESModule)
//         .withModuleSplitStyle(
//           ModuleSplitStyle.SmallModulesFor(List("semagrams-graph-app"))
//         )
//     },
//   )
//   .dependsOn(core)
//   .enablePlugins(ScalaJSPlugin)

lazy val petri_app = (project in file("apps/petri"))
  .settings(
    name := "semagrams-petri-app",
    // scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("semagrams-petri-app"))
        )
    },
  )
  .dependsOn(core)
  .enablePlugins(ScalaJSPlugin)

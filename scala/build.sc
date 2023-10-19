import mill._
import mill.scalalib._
import mill.scalajslib._
import mill.scalajslib.api._
import mill.scalalib.publish._
import scalafmt._

def defaultPomSettings(desc: String) = PomSettings(
  description = desc,
  organization = "org.algebraicjulia",
  url = "https://github.com/AlgebraicJulia/Semagrams.jl",
  licenses = Seq(License.MIT),
  versionControl = VersionControl.github("AlgebraicJulia", "Semagrams.jl"),
  developers = Seq(
    Developer("olynch", "Owen Lynch", "https://owenlynch.org")
  )
)

trait Defaults extends ScalaJSModule with PublishModule with ScalafmtModule {
  def scalaVersion = "3.3.0"
  def scalaJSVersion = "1.13.2"
  def ammoniteVersion = "3.0.0-M0-53-084f7f4e"

  def scalacOptions = Seq("-deprecation", "-feature", "-Wunused:all")

  def moduleKind = T { ModuleKind.ESModule }

  def desc: String

  def pomSettings = defaultPomSettings(desc)

  def publishVersion = "0.3.0-SNAPSHOT"

  def sonatypeUri = "https://s01.oss.sonatype.org/service/local"

  def sonatypeSnapshotUri =
    "https://s01.oss.sonatype.org/content/repositories/snapshots"
}

object acsets extends Defaults {
  def desc = "A flexible category theoretic in-memory database"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::3.1.3",
    ivy"org.typelevel::cats-core::2.10.0",
    ivy"org.typelevel::cats-kernel::2.10.0"
  )

  def artifactName = "acsets"

  object test extends ScalaTests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")

    def testFramework = "utest.runner.Framework"
  }
}

object balloons extends Defaults {
  def desc = "A library for state machines with cats-effect and laminar"

  def ivyDeps = Agg(
    ivy"com.raquo::laminar::16.0.0",
    ivy"org.typelevel::cats-core::2.10.0",
    ivy"org.typelevel::cats-kernel::2.10.0",
    ivy"org.typelevel::cats-effect::3.5.1",
    ivy"org.typelevel::cats-effect-cps::0.4.0"
  )

  def artifactName = "balloons"

  object test extends ScalaJSTests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats::0.8.3"
    )

    // def jsEnvConfig: T[JsEnvConfig] = T { JsEnvConfig.JsDom() }

    def testFramework = "weaver.framework.CatsEffect"
  }
}

object core extends Defaults {
  def desc = "A library for semantic diagrams"

  def artifactName = "semagrams"

  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::2.6.0",
    ivy"com.raquo::laminar::16.0.0",
    ivy"com.lihaoyi::upickle::3.1.3",
    ivy"org.typelevel::cats-core::2.10.0",
    ivy"org.typelevel::cats-kernel::2.10.0",
    ivy"org.typelevel::cats-effect::3.5.1",
    ivy"org.typelevel::cats-effect-cps::0.4.0",
    ivy"com.github.japgolly.scalacss::core::1.0.0",
    ivy"dev.optics::monocle-core::3.2.0",
    ivy"dev.optics::monocle-macro::3.2.0"
  )

  object test extends ScalaTests {
    def jsEnvConfig = T(JsEnvConfig.JsDom())

    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.8.1")

    def testFramework = "utest.runner.Framework"
  }
}

trait SemagramsApp extends Defaults {
  def moduleDeps = Seq(core)
}

object apps extends Module {

  // object simplepetri extends SemagramsApp {
  //   def desc = "simple petri editor"

  //   def artifactName = "semagrams-simplepetri"
  // }

  // object dwd extends SemagramsApp {
  //   def desc = "A string diagram editor"

  //   def artifactName = "semagrams-dwd"
  // }

  object graph extends SemagramsApp {
    def desc = "graph editor"

    def artifactName = "semagrams-graph"
  }

}

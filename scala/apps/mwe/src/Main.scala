package semagrams.mwe

import semagrams._
import semagrams.api._
import semagrams.acsets._

import com.raquo.laminar.api.L._
import cats.effect.IO
import scala.scalajs.js.annotation.JSExportTopLevel



case object Node extends Ob
case object parent extends HomWithDom {
  val dom = Node
  val codom = Node
}
case object label extends AttrWithDom with PValue[String] {
  val dom = Node
}


case object MWESchemaOb extends StaticSchema {
  val schema = BasicSchema(Node,parent,label)
}

type MWESchema = MWESchemaOb.type

type MWE = ACSet[MWESchema]
object MWE {
  def apply() = ACSet[MWESchema]()
}

def bindings(es: EditorState, g: Var[MWE]) = {
  val a = Actions(es, g)

  Seq(
    clickOn(MouseButton.Left).flatMap(ent => a.log("clicked:" + ent)),
    dblClickOn(MouseButton.Left).flatMap(_ => a.add(Node))
  )
}

object Main {
  @JSExportTopLevel("MWEApp")
  object MWEApp extends Semagram {

    def run(es: EditorState): IO[Unit] = {
      for {
        g <- IO(Var(MWE()))
        lg <- IO(
          g.signal
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Node, BasicDisc(es)),
            // ACSetEdgeSource(E, Src, Tgt, BasicArrow(es))
          )
        )
        _ <- es.bindForever(bindings(es, g))
      } yield ()
    }
  }
}

package semagrams.simplepetri

import semagrams.api._
import semagrams.acsets.{_, given}
import Petris._
import cats.data._
import cats.implicits._
import scala.collection.mutable

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._

/**
 * TODO:
 *
 * - [X] Janky automatic layout
 * - [X] Fix focus bug (can't figure out)
 * - Write help popup
 * - Generate equations
 * - JSON export
 */

val conversionSpec = Seq(
  (S, "S", Seq((Content, "sname"))),
  (T, "T", Seq((Content, "tname"))),
  (I, "I", Seq((IS, "is"), (IT, "it"))),
  (O, "O", Seq((OS, "os"), (OT, "ot"))),
)

def petriFromCLJson(s: String): ACSet = {
  val cljson = read[catlab.ACSet](s)
  val mkPetri: State[ACSet, Unit] = conversionSpec.traverse_(
    {
      case (ob, obS, propSpecs) => {
        val propses = cljson(obS).map(
          m => {
            propSpecs.foldLeft(PropMap())(
              {
                case (pm, (p, pS)) => pm.set(p, read[p.Value](m(pS))(p.rw))
              }
            )
          }
        )
        addParts(ROOT, ob, propses)
      }
    }
  )
  springLayoutPetri(
    mkPetri.run(Petri()).value._1,
    BoundingBox(Complex(100,100), Complex(800, 800))
  )
}

def clJsonFromPetri(petri: ACSet): String = {
  val indexConversion = conversionSpec
    .map[Ob](_._1)
    .map(ob => ob -> petri.partsOnly(ROOT, ob).zipWithIndex.toMap).toMap
  def juliaIndex(p: Part) = {
    val (ob, i) = p.path(0)
    indexConversion(ob)(p)
  }
  val cljson = conversionSpec.map(
    {
      case (ob, obS, props) =>
        (obS -> petri.parts(ROOT, ob).map(
           { case (i, a) => props.map(
              {
                case (f: Hom, fS) => (fS -> ujson.Num(juliaIndex(a.props(f))))
                case (f, fS) =>
                  (fS ->
                     a.props.get(f).map(
                       x => ujson.Str(x.asInstanceOf[String])
                     ).getOrElse(ujson.Null)
                  )
              }
            ).toMap }
         ))
    }
  ).toMap
  write(cljson)
}


val springLength = 70.0
val springConstant = 0.1

def springLayoutPetri(petri: ACSet, inside: BoundingBox): ACSet = {
  val ps = mutable.Map[Part, Complex]()
  val dps = mutable.Map[Part, Complex]()
  for ((s,_) <- petri.parts(ROOT, S)) {
    ps += (s -> inside.sample())
  }
  for ((t,_) <- petri.parts(ROOT, T)) {
    ps += (t -> inside.sample())
  }
  def springDiff(s: Part, t: Part, l: Double, c: Double): Unit = {
    val diff = ps(s) - ps(t)
    val absf = diff.abs - l
    val dp = diff.normalize * absf * c
    dps += (s -> (dps(s) - dp))
    dps += (t -> (dps(t) + dp))
  }
  def repelDiff(s: Part, t: Part, l: Double, c: Double): Unit = {
    val diff = ps(s) - ps(t)
    val absf = (diff.abs - l) min 0
    val dp = diff.normalize * absf * c
    dps += (s -> (dps(s) - dp))
    dps += (t -> (dps(t) + dp))
  }
  def computeDiffs(): Unit = {
    for ((s,_) <- petri.parts(ROOT, S)) {
      dps += (s -> 0)
    }
    for ((t,_) <- petri.parts(ROOT, T)) {
      dps += (t -> 0)
    }
    for ((_,a) <- petri.parts(ROOT, I)) {
      springDiff(a.props(IS), a.props(IT), springLength, springConstant)
    }
    for ((_,a) <- petri.parts(ROOT, O)) {
      springDiff(a.props(OS), a.props(OT), springLength, springConstant)
    }
    for ((i,_) <- petri.parts(ROOT, S) ++ petri.parts(ROOT, T)) {
      for ((j,_) <- petri.parts(ROOT, S) ++ petri.parts(ROOT, T)) {
        if (i != j) {
          repelDiff(j, i, springLength * 2, springConstant / 2)
        }
      }
    }
  }
  def step(): Unit = {
    for ((s,_) <- petri.parts(ROOT, S)) {
      ps += (s -> (ps(s) + dps(s)))
    }
    for ((t,_) <- petri.parts(ROOT, T)) {
      ps += (t -> (ps(t) + dps(t)))
    }
  }
  computeDiffs()
  var n = 1000
  while (dps.values.map(_.abs).foldLeft(0.0)((x, y) => x max y) > 0.1 && n > 0) {
    step()
    computeDiffs()
    n -= 1
  }
  ps.foldLeft(petri)((acs, pv) => acs.setSubpart(pv._1, Center, pv._2))
}

case class EquationWindow() extends Entity {
  val ty = EquationWindow
}

object EquationWindow extends EntityType
  

def bindings(es: EditorState, g: UndoableVar[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui, clJsonFromPetri, petriFromCLJson)

  Seq(
    keyDown("s").andThen(a.addAtMouse(S)),
    keyDown("t").andThen(a.addAtMouse(T)),
    keyDown("d").andThen(a.del),
    keyDown("E")
      .withMods(KeyModifier.Shift)
      .andThen(a.importExport),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    clickOnPart(MouseButton.Left, PartType(Seq(S)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(I, IS, IT)),
    clickOnPart(MouseButton.Left, PartType(Seq(T)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(O, OT, OS)),
    clickOnPart(MouseButton.Left, PartType(Seq(S))).withMods().flatMap(a.drag),
    clickOnPart(MouseButton.Left, PartType(Seq(T))).withMods().flatMap(a.drag),
    keyDown("e").andThen(for {
                           mx <- es.hoveredPart(Seq(PartType(Seq(S)), PartType(Seq(T))))
                           _ <- mx.map(x => a.edit(Content, false)(x)).getOrElse(IO(()))
                         } yield ()),
    dblClickOnPart(MouseButton.Left, PartType(Seq(S))).flatMap(a.edit(Content, false)),
    dblClickOnPart(MouseButton.Left, PartType(Seq(T))).flatMap(a.edit(Content, false))
  )
}

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      for {
        // initg <- IO(init
        //               .map(s => petriFromCLJson(read[catlab.ACSet](s)))
        //               .getOrElse(Petri()))
        initg <- IO(Petri())
        sprungg <- IO(
          springLayoutPetri(
            initg,
            BoundingBox(Complex(50,50), Complex(400,400)))
        )
        g <- IO(UndoableVar(sprungg))
        lg <- IO(
          g.signal.map(assignBends(Map(I -> (IS, IT), O -> (OT, OS)), 0.5))
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(S, BasicDisc(es)),
            ACSetEntitySource(T, BasicRect(es)),
            ACSetEdgeSource(I, IS, IT, BasicArrow(es)),
            ACSetEdgeSource(O, OT, OS, BasicArrow(es)),
          )
        )
        ui <- es.makeUI()
        _ <- ui.addHtmlEntity(
          EquationWindow(),
          () => PositionWrapper(Position.botMid(15), massActionTypeset(g.signal))
        )
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}

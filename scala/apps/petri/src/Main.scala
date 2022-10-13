package semagrams.petri

import cats.Monad
import cats.data.OptionT
import cats.data._
import cats.effect.IO
import cats.effect.syntax.all._
import com.raquo.laminar.api.L.{_, given}
import org.scalajs.dom
import semagrams._
import semagrams.acsets.{_, given}
import semagrams.actions._
import semagrams.controllers._

import monocle._
import monocle.macros.GenIso

import semagrams.sprites._
import semagrams.text._
import semagrams.util._
import semagrams.widgets._
import upickle.default._

import scala.collection.MapView.Keys
import scala.scalajs.js.annotation.JSExportTopLevel

import Petris._
import org.scalajs.dom.KeyboardEvent
import com.raquo.laminar.nodes.ReactiveHtmlElement

case class StratPetri(acset: BareACSet)

case object Stratification extends AttrType {
  type Value = Set[String]
}

case object StratificationWith extends Attr[S.type, Set[String]] {
  val dom = S
  val codom = Stratification
}

case object TransitionTypeValue extends AttrType {
  type Value = Option[String]
}

case object TransitionType extends Attr[T.type, Option[String]] {
  val dom = T
  val codom = TransitionTypeValue
}

object StratPetri {
  def ops = new ACSetOps[StratPetri] {
    given acsetInstance: ACSet[StratPetri] with
      val bare = GenIso[StratPetri, BareACSet]
      val schema = LabelledReactionNet.ops.schema.extend(
        StratificationWith,
        TransitionType
      )
  }
}

given ACSet[StratPetri] = StratPetri.ops.acsetInstance

type PropPetri = WithProps[StratPetri]

object PropPetriOps extends PropOps[StratPetri] {
  val acsetInstance = WithProps.ops[StratPetri].acsetInstance
}

import PropPetriOps._

val ops = Action.ops[PropPetri]

val addSpecies = addEntityPos[StratPetri, S.type](
  S,
  v =>
    for {
      _ <- setSubpart(SName, v, "")
      _ <- setSubpart(Concentration, v, 1.0)
      _ <- setSubpart(StratificationWith, v, Set())
    } yield ()
)

val addTransition = addEntityPos[StratPetri, T.type](
  T,
  v =>
    for {
      _ <- setSubpart(TName, v, "")
      _ <- setSubpart(Rate, v, 1.0)
      _ <- setSubpart(TransitionType, v, None)
    } yield ()
)

def flexcol(mods: Modifier[ReactiveHtmlElement.Base]*) = {
  div(
    display := "flex",
    flex := "1",
    flexDirection := "column",
    mods
  )
}

def flexrow(mods: Modifier[ReactiveHtmlElement.Base]*) = {
  div(
    display := "flex",
    flex := "1",
    flexDirection := "row",
    mods
  )
}

def makeInput[S, A](
    oftype: String,
    $state: Var[S],
    lens: Lens[S, A],
    prism: Prism[String, A]
) = input(
  typ := oftype,
  value := prism.reverseGet(lens.get($state.now())),
  onInput.stopPropagation.mapToValue --> $state.updater((s, str: String) =>
    prism.getOption(str) match {
      case Some(a) => lens.replace(a)(s)
      case None    => s
    }
  ),
  onKeyDown.filter(_.key != "Escape").stopPropagation --> Observer((_) => ())
)

def setChecked[S]($state: Var[S], lens: Lens[S, Boolean]) = Seq(
  checked := lens.get($state.now()),
  onInput.mapToChecked --> $state.updater((s, c: Boolean) => lens.replace(c)(s))
)

def parseDouble(s: String) = {
  val normalized = if (s == "") {
    "0"
  } else if (s.charAt(s.length() - 1) == '.') {
    s + "0"
  } else {
    s
  }
  try {
    Some(normalized.toDouble)
  } catch {
    case e: NumberFormatException => None
  }
}

def setIfLens[A](a: A) =
  Lens[A, Boolean](_ == a)(b => oldA => if b then a else oldA)

def ifInLens[A](a: A) = Lens[Set[A], Boolean](_ contains a)(b =>
  setA => if b then setA + a else setA - a
)

object EditorParams {
  val dims = Complex(400, 120)
  val paddingV = 4.0
  val borderV = 2.5

  def toplevelStyle = Seq(
    borderStyle := "solid",
    height := s"${dims.y - borderV*2}px",
    width := s"${dims.x - borderV*2}px",
    borderWidth := s"${borderV}px",
    backgroundColor := "white",
  )
}

def inputCell(labelV: String, el: Element) =
  import EditorParams._
  flexcol(
    padding := s"${paddingV}px",
    label(labelV),
    el
  )

def speciesEditor($p: Var[PropPetri], s: Elt[S.type], eltDims: Complex) = {
  import EditorParams._

  val pos = Complex(eltDims.x / 2, eltDims.y)
    - Complex(dims.x / 2, dims.y)

  wrappedHtml(
    flexcol(
      toplevelStyle,
      flexrow(
        inputCell(
          "Label:",
          makeInput("text", $p, PropPetriOps.subpartLens(SName, s), Prism.id)
        ),
        inputCell(
          "Concentration:",
          makeInput(
            "text",
            $p,
            PropPetriOps.subpartLens(Concentration, s),
            Prism(parseDouble)(_.toString)
          )
        )
      ),
      flexcol(
        padding := s"${paddingV}px",
        "Do stratification with:",
        flexrow(
          Seq("infect", "disease", "strata").map(tt =>
            flexrow(
              alignItems := "center",
              input(
                typ := "checkbox",
                setChecked(
                  $p,
                  PropPetriOps
                    .subpartLens(StratificationWith, s)
                    .andThen(ifInLens(tt))
                )
              ),
              label(tt)
            ),
          )
        )
      )
    ),
    pos,
    dims
  )
}

def transitionEditor($p: Var[PropPetri], t: Elt[T.type], eltDims: Complex) = {
  import EditorParams._

  val pos = Complex(eltDims.x / 2, eltDims.y)
    - Complex(dims.x / 2, dims.y)

  wrappedHtml(
    flexcol(
      flexrow(
        inputCell(
          "Label:",
          makeInput("text", $p, PropPetriOps.subpartLens(TName, t), Prism.id)
        ),
        inputCell(
          "Rate:",
          makeInput(
            "text",
            $p,
            PropPetriOps.subpartLens(Rate, t),
            Prism(parseDouble)(_.toString)
          )
        )
      ),
      flexcol(
        padding := s"${padding}px",
        "Transition Type:",
        form(
          fieldSet(
            borderStyle := "none",
            flexrow(
              Seq("infect", "disease", "strata").map(tt =>
                flexrow(
                  input(
                    typ := "radio",
                    value := tt,
                    name := "type",
                    setChecked(
                      $p,
                      PropPetriOps
                        .subpartLens(TransitionType, t)
                        .andThen(setIfLens(Some(tt)))
                    )
                  ),
                  label(tt)
                ),
              )
            )
          )
        )
      )
    ),
    pos,
    dims
  )
}

def openSpeciesEditor(s: Elt[S.type]): Action[PropPetri, Unit] = for {
  $p <- ops.ask.map(_.$model)
  eltDims <- ops.ask.map(_.dims())
  ed <- ops.delay(speciesEditor($p, s, eltDims))
  _ <- addChild(ed)
  bindables <- ops.ask.map(_.bindables)
  _ <- (for {
    _ <- Bindings[PropPetri, Unit](keyDown("Escape")).run
    _ <- removeChild(ed)
  } yield ()).start
} yield ()

def openTransitionEditor(t: Elt[T.type]): Action[PropPetri, Unit] = for {
  $p <- ops.ask.map(_.$model)
  eltDims <- ops.ask.map(_.dims())
  ed <- ops.delay(transitionEditor($p, t, eltDims))
  _ <- addChild(ed)
  bindables <- ops.ask.map(_.bindables)
  _ <- (for {
    _ <- Bindings[PropPetri, Unit](keyDown("Escape")).run
    _ <- removeChild(ed)
  } yield ()).start
} yield ()

val helpText = """
Keys:
s: add species
t: add transition
d: delete hovered species/transition/arrow
h/?: toggle quick help
p: tutorial

Click and drag to move around species/transitions

Double-click to modify species/transitions

Shift-click and drag to add arrows
""".linesIterator.toSeq

val bindings = Bindings[PropPetri, Unit](
  keyDown("s").andThen(addSpecies.flatMap(openSpeciesEditor)),
  keyDown("t").andThen(addTransition.flatMap(openTransitionEditor)),
  keyDown("d").andThen(remEntity),
  keyDown("h").andThen(showPopoverUntil(helpText, keyDown("h"))),
  keyDown("?").andThen(showPopoverUntil(helpText, keyDown("?"))),
  clickOn(ClickType.Single, MouseButton.Left, S)
    .withMods(KeyModifier.Shift)
    .flatMap(s => dragEdge[StratPetri, I.type, S.type, T.type](IS, IT, s)),
  clickOn(ClickType.Single, MouseButton.Left, T)
    .withMods(KeyModifier.Shift)
    .flatMap(t => dragEdge[StratPetri, O.type, T.type, S.type](OT, OS, t)),
  clickOn(ClickType.Double, MouseButton.Left, S)
    .flatMap(openSpeciesEditor),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
  clickOn(ClickType.Double, MouseButton.Left, T)
    .flatMap(openTransitionEditor),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
)

def arcExtractor(p: PropPetri, sprites: Sprites) = {
  val bends = assignBends(List((I, IS, IT, 1), (O, OS, OT, -1)), p, 0.3)
  val inputs = edgeExtractor(IS, IT)(p, sprites, bends)
  val outputs = edgeExtractor(OT, OS)(p, sprites, bends)
  inputs ++ outputs
}

val colors = Map(
  Some("infect") ->  "#a08fae",
  Some("disease") -> "#ffeec6",
  Some("strata") ->  "#a8dcd9",
  None -> "#E28F41"
)

def renderPetri(
    $petri: Var[PropPetri],
    hover: HoverController,
    drag: DragController,
    mouse: MouseController
) = {

  val spriteMaps = SpriteMaps[PropPetri](
    $petri.signal,
    List(
      SpriteMaker[PropPetri](
        Disc(),
        (s, _) =>
          s.parts(S)
            .toList
            .map(v =>
              (
                v,
                s.subpart(Props(S), v).get
                  + (Content, s.subpart(SName, v).getOrElse(""))
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#6C9AC3")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Style, "filter: opacity(0.7)")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Box(),
        (s, _) =>
          s.parts(T)
            .toList
            .map(v =>
              (
                v,
                s.subpart(Props(T), v).get
                  + (Content, s.subpart(TName, v).getOrElse(""))
                  + (Fill, colors(s.subpart(TransitionType, v).get))
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#E28F41")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Style, "filter: opacity(0.7)")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Arrow(),
        arcExtractor,
        Stack(
          WithDefaults(PropMap() + (Stroke, "black")),
          Shorten(5),
          Hoverable(hover, MainHandle, PropMap() + (Stroke, "lightgray"))
        )
      )
    )
  )

  svg.g(
    spriteMaps.attach
  )
}

val serializer = PropPetriOps.acsetInstance.rw(
  AttrTypeSerializers()
    + ATRW(PropValue, PropMap.rw)
    + ATRW(NameValue, summon[ReadWriter[String]])
    + ATRW(RateValue, summon[ReadWriter[Double]])
    + ATRW(ConcentrationValue, summon[ReadWriter[Double]])
    + ATRW(Stratification, summon[ReadWriter[Set[String]]])
    + ATRW(TransitionTypeValue, summon[ReadWriter[Option[String]]])
)

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      drag <- ops.ask.map(_.drag)
      mouse <- ops.ask.map(_.mouse)
      _ <- addChild(renderPetri($model, hover, drag, mouse))
      _ <- bindings.runForever
    } yield ()

    mountWithAction(el, WithProps[StratPetri](), serializer, action)
  }
}

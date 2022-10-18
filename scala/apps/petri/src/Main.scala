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

import scalacss.DevDefaults.StyleA
import scalacss.internal.mutable.GlobalRegistry

implicit def applyStyle(styleA: StyleA): Mod[HtmlElement] =
  cls := styleA.className.value

case object StratificationWith extends AttrWithDom with PValue[Set[String]] {
  val dom = S
}

case object TransitionType extends AttrWithDom with PValue[Option[String]] {
  val dom = T
}

case object SchStratPetri extends StaticSchema {
  val schema = SchLabelledReactionNet.extend(StratificationWith, TransitionType)
}

type StratPetri = ACSet[SchStratPetri.type]
object StratPetri {
  def apply() = ACSet[SchStratPetri.type]()
}

val ops = Action.ops[StratPetri]
val aops = summon[ACSetOps[SchStratPetri.type]]

val addSpecies = addPartPos[SchStratPetri.type](
  S,
  PropMap()
    .set(SName, "")
    .set(Concentration, 1.0)
    .set(StratificationWith, Set())
)

val addTransition = addPartPos[SchStratPetri.type](
  T,
  PropMap().set(TName, "").set(Rate, 1.0).set(TransitionType, None)
)

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
    height := s"${dims.y - borderV * 2}px",
    width := s"${dims.x - borderV * 2}px",
    borderWidth := s"${borderV}px",
    backgroundColor := "white"
  )
}

def inputCell(labelV: String, el: Element) =
  import EditorParams._
  div(
    PS.col,
    padding := s"${paddingV}px",
    label(labelV),
    el
  )

def speciesEditor($p: Var[StratPetri], s: Part, eltDims: Complex) = {
  import EditorParams._

  val pos = Complex(eltDims.x / 2, eltDims.y)
    - Complex(dims.x / 2, dims.y)

  wrappedHtml(
    div(
      PS.col,
      toplevelStyle,
      div(
        PS.row,
        inputCell(
          "Label:",
          makeInput("text", $p, aops.subpartLens(SName, s), Iso.id)
        ),
        inputCell(
          "Concentration:",
          makeInput(
            "text",
            $p,
            aops.subpartLens(Concentration, s),
            Prism(parseDouble)(_.toString)
          )
        )
      ),
      div(
        PS.col,
        padding := s"${paddingV}px",
        "Do stratification with:",
        div(
          PS.row,
          Seq("infect", "disease", "strata").map(tt =>
            div(
              PS.row,
              alignItems := "center",
              input(
                typ := "checkbox",
                setChecked(
                  $p,
                  aops
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

def transitionEditor($p: Var[StratPetri], t: Part, eltDims: Complex) = {
  import EditorParams._

  val pos = Complex(eltDims.x / 2, eltDims.y)
    - Complex(dims.x / 2, dims.y)

  wrappedHtml(
    div(
      PS.col,
      toplevelStyle,
      div(
        PS.row,
        inputCell(
          "Label:",
          makeInput("text", $p, aops.subpartLens(TName, t), Iso.id)
        ),
        inputCell(
          "Rate:",
          makeInput(
            "text",
            $p,
            aops.subpartLens(Rate, t),
            Prism(parseDouble)(_.toString)
          )
        )
      ),
      div(
        PS.col,
        padding := s"${padding}px",
        "Transition Type:",
        form(
          fieldSet(
            borderStyle := "none",
            div(
              PS.row,
              Seq("infect", "disease", "strata").map(tt =>
                div(
                  PS.row,
                  input(
                    typ := "radio",
                    value := tt,
                    name := "type",
                    setChecked(
                      $p,
                      aops
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

def openSpeciesEditor(s: Part): Action[StratPetri, Unit] = for {
  $p <- ops.ask.map(_.$model)
  eltDims <- ops.ask.map(_.dims())
  ed <- ops.delay(speciesEditor($p, s, eltDims))
  _ <- addChild(ed)
  bindables <- ops.ask.map(_.bindables)
  _ <- (for {
    _ <- Bindings[StratPetri, Unit](keyDown("Escape")).run
    _ <- removeChild(ed)
  } yield ()).start
} yield ()

def openTransitionEditor(t: Part): Action[StratPetri, Unit] = for {
  $p <- ops.ask.map(_.$model)
  eltDims <- ops.ask.map(_.dims())
  ed <- ops.delay(transitionEditor($p, t, eltDims))
  _ <- addChild(ed)
  bindables <- ops.ask.map(_.bindables)
  _ <- (for {
    _ <- Bindings[StratPetri, Unit](keyDown("Escape")).run
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

val bindings = Bindings[StratPetri, Unit](
  keyDown("s").andThen(
    addSpecies.flatMap(openSpeciesEditor).flatMap(_ => update)
  ),
  keyDown("t").andThen(
    addTransition.flatMap(openTransitionEditor).flatMap(_ => update)
  ),
  keyDown("d").andThen(remPart),
  keyDown("h").andThen(showPopoverUntil(helpText, keyDown("h"))),
  keyDown("?").andThen(showPopoverUntil(helpText, keyDown("?"))),
  clickOn(ClickType.Single, MouseButton.Left, S)
    .withMods(KeyModifier.Shift)
    .flatMap(s => dragEdge(I, IS, IT, s.asInstanceOf[Part])),
  clickOn(ClickType.Single, MouseButton.Left, T)
    .withMods(KeyModifier.Shift)
    .flatMap(t => dragEdge(O, OT, OS, t.asInstanceOf[Part])),
  clickOn(ClickType.Double, MouseButton.Left, S)
    .flatMap(e => openSpeciesEditor(e.asInstanceOf[Part])),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(e =>
    dragPart(e.asInstanceOf[Part])
  ),
  clickOn(ClickType.Double, MouseButton.Left, T)
    .flatMap(e => openTransitionEditor(e.asInstanceOf[Part])),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(e =>
    dragPart(e.asInstanceOf[Part])
  )
)

def arcExtractor(p: StratPetri, sprites: Sprites) = {
  val bends = assignBends(List((I, IS, IT, 1), (O, OS, OT, -1)), p, 0.3)
  val inputs = edgeExtractor(I, IS, IT)(p, sprites, bends)
  val outputs = edgeExtractor(O, OT, OS)(p, sprites, bends)
  inputs ++ outputs
}

val colors = Map(
  Some("infect") -> "#a08fae",
  Some("disease") -> "#ffeec6",
  Some("strata") -> "#a8dcd9",
  None -> "#E28F41"
)

def renderPetri(
    $petri: Var[StratPetri],
    hover: HoverController,
    mouse: MouseController
) = {

  val spriteMaps = SpriteMaps[StratPetri](
    $petri.signal,
    List(
      SpriteMaker[StratPetri](
        Disc(),
        (s, _) =>
          s.parts(S)
            .toList
            .map(v =>
              (
                v,
                s.props(v)
                  + (Content, s.trySubpart(SName, v).getOrElse(""))
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
          Hoverable(
            hover,
            MainHandle,
            PropMap() + (Style, "filter: opacity(0.7)")
          ),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[StratPetri](
        Box(),
        (s, _) =>
          s.parts(T)
            .toList
            .map(v =>
              (
                v,
                s.props(v)
                  + (Content, s.trySubpart(TName, v).getOrElse(""))
                  + (Fill, colors(s.subpart(TransitionType, v)))
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
          Hoverable(
            hover,
            MainHandle,
            PropMap() + (Style, "filter: opacity(0.7)")
          ),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[StratPetri](
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

val serializer = ACSet.rw[SchStratPetri.type]

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- addChild(renderPetri($model, hover, mouse))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head").appendChild(styleTag(PSrendered).ref)

    plutoMain(el, StratPetri(), serializer, action)
  }
}

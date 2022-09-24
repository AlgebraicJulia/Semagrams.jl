package petri

import semagrams._
import semagrams.util._
import semagrams.acsets.{*, given}
import semagrams.actions._
import semagrams.text._
import cats.data._
import cats.Monad
import cats.effect.IO
import cats.data.OptionT
import semagrams.sprites._
import com.raquo.laminar.api.L.{*, given}
import semagrams.controllers._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import upickle.default._

import Petris._
import scala.collection.MapView.Keys

type PropPetri = WithProps[LabelledReactionNet]
type M[T] = Action[PropPetri, T]

val arcLoop: Action[PropPetri, Unit] =
  loopDuringPress(
    "Shift",
    for {
      v <- fromMaybe(
        Bindings(
          clickOn(ClickType.Single, MouseButton.Left, S).map(Left(_)),
          clickOn(ClickType.Single, MouseButton.Left, T).map(Right(_))
        ).run
      )
      _ <- v match {
        case Left(s) =>
          dragEdge[LabelledReactionNet, I.type, S.type, T.type](IS, IT, s)
        case Right(t) =>
          dragEdge[LabelledReactionNet, O.type, T.type, S.type](OT, OS, t)
      }
    } yield {}
  )

val modifyRates: Action[PropPetri, Unit] =
  loopDuringPress(
    "Control",
    for {
      v <- fromMaybe(
        Bindings(
          clickOn(ClickType.Single, MouseButton.Left, S).map(Left(_)),
          clickOn(ClickType.Single, MouseButton.Left, T).map(Right(_))
        ).run
      )
      _ <- v match {
        case Left(s)  => dragControl[PropPetri, S.type](Concentration, 0.1)(s)
        case Right(t) => dragControl[PropPetri, T.type](Rate, 0.01)(t)
      }
    } yield {}
  )

val addSpecies = addEntityPos[LabelledReactionNet, S.type](
  S,
  v =>
    for {
      _ <- setSubpart(SName, v, "")
      _ <- setSubpart(Concentration, v, 1.0)
    } yield ()
)

val addTransition = addEntityPos[LabelledReactionNet, T.type](
  T,
  v =>
    for {
      _ <- setSubpart(TName, v, "")
      _ <- setSubpart(Rate, v, 1.0)
    } yield ()
)

def dragInputLoop(s: Elt[S.type], t: Elt[T.type]): Action[PropPetri, Unit] =
  for {
    _ <- showTip(
      "Now, hold Shift, and drag on the species to make an arrow; drop the arrow to the transition."
    )
    _ <- Bindings(keyDown("Shift")).run
    _ <- loopDuringPress(
      "Shift",
      for {
        s <- fromMaybe(
          Bindings(clickOn(ClickType.Single, MouseButton.Left, S)).run
        )
        _ <- dragEdge[LabelledReactionNet, I.type, S.type, T.type](IS, IT, s)
      } yield ()
    )
    $model <- getModel
    _ <-
      if (
        $model.now()
          .incident(IS, s)
          .intersect($model.now().incident(IT, t))
          .nonEmpty
      ) {
        Action.pure[PropPetri, Unit](())
      } else {
        for {
          _ <- showTip("Whoops, try again! (Enter to continue)")
          _ <- Bindings(keyDown("Enter")).run
          _ <- dragInputLoop(s, t)
        } yield ()
      }
  } yield ()

def dragControlLoop[X <: Ob](
    s: Elt[X],
    attr: Attr[X, Double],
    texts: String*
): Action[PropPetri, Unit] = for {
  $model <- getModel
  init <- Action.pure($model.now().subpart(attr, s))
  _ <- showTip(texts*)
  _ <- Bindings(keyDown("Control").andThen(modifyRates)).run
  _ <-
    if ($model.now().subpart(attr, s) == init) {
      for {
        _ <- showTip("Whoops, try again! (Enter to continue)")
        _ <- Bindings(keyDown("Enter")).run
        _ <- dragControlLoop(s, attr, texts*)
      } yield ()
    } else {
      Action.pure[PropPetri, Unit](())
    }
} yield ()

def tutorial: Action[PropPetri, Unit] = for {
  _ <- updateModel[PropPetri](_ => WithProps[LabelledReactionNet]())
  _ <- update
  _ <- showTip(
    "Hi, welcome to the Semagrams Petri net tutorial! (Enter to continue)"
  )
  _ <- Bindings(keyDown("Enter")).run
  _ <- showTip(
    "Let's get started! Put your mouse anywhere in this box and press \"s\" to add a species."
  )
  s <- Bindings(keyDown("s").andThen(addSpecies)).run.map(_.get)
  _ <- showTip(
    "Great job! Now type a name and hit escape when you're done."
  )
  _ <- editStringAttrBlocking(S, SName)(s)
  _ <- showTip("You rock! (Enter to continue)")
  _ <- Bindings(keyDown("Enter")).run
  _ <- showTip(
    "You should see down below that the plot has updated. (Enter to continue)"
  )
  _ <- Bindings(keyDown("Enter")).run
  _ <- showTip(
    "But the plot is just a straight line: let's make it more interesting. (Enter to continue)"
  )
  _ <- Bindings(keyDown("Enter")).run
  _ <- showTip("Move your mouse somewhere else and add a transition with \"t\".")
  t <- Bindings(keyDown("t").andThen(addTransition)).run.map(_.get)
  _ <- showTip("Don't forget to give it a name!")
  _ <- editStringAttrBlocking(T, TName)(t)
  _ <- showTip("What a pro! (Enter to continue)")
  _ <- Bindings(keyDown("Enter")).run
  _ <- dragInputLoop(s, t)
  _ <- showTip(
    "You should see a nice exponential decay curve in the graph now. (Enter to continue)"
  )
  _ <- Bindings(keyDown("Enter")).run
  _ <- dragControlLoop(
    s,
    Concentration,
    "Now we're going to edit the concentration of that species you added.",
    "To do this, hold control and drag on the circle."
  )
  _ <- showTip("Fantastic! (Enter to continue)")
  _ <- Bindings(keyDown("Enter")).run
  _ <- dragControlLoop(
    t,
    Rate,
    "You can do the same thing to edit the rate of the transition; try it out!"
  )
  _ <- showTip(
    "You can drag the species and transition around to reposition them.",
    "You can also rename by double-clicking",
    "Try it out, and then hit enter to continue when you're ready."
  )
  _ <- loopUntilPress(
    "Enter",
    Bindings[PropPetri, Unit](
      clickOn(ClickType.Double, MouseButton.Left, S)
        .flatMap(editStringAttr(S, SName)),
      clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
      clickOn(ClickType.Double, MouseButton.Left, T)
        .flatMap(editStringAttr(T, TName)),
      clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
    ).run.map(_ => ())
  )
  _ <- showTip(
    "Finally, you can delete anything by hovering over it and pressing \"d\"",
    "Go ahead and destroy all your hard work. (Enter to continue)"
  )
  _ <- loopUntilPress(
    "Enter",
    Bindings[PropPetri, Unit](
      keyDown("d").andThen(remEntity),
    ).run.map(_ => ())
  )
  _ <- showTip(
    "And that's pretty much it!",
    "Remember, you can access the tutorial again with \"p\", or a quick reference with \"h\".",
    "Hit enter to get rid of this, and enable all keybindings."
  )
  _ <- Bindings(keyDown("Enter")).run
  _ <- hideTip
} yield {}

val helpText = """
Keys:
s: add species
t: add transition
d: delete hovered species/transition/arrow
h: toggle quick help
p: tutorial

Click and drag to move around species/transitions

Double-click to modify labels

Shift-click and drag to add arrows

Control-click and drag to modify rates/concentrations
""".linesIterator.toSeq

val bindings = Bindings[PropPetri, Unit](
  keyDown("s").andThen(addSpecies.flatMap(editStringAttr(S, SName))),
  keyDown("t").andThen(addTransition.flatMap(editStringAttr(T, TName))),
  keyDown("d").andThen(remEntity),
  keyDown("Shift").andThen(arcLoop),
  keyDown("Control").andThen(modifyRates),
  keyDown("h").andThen(showPopoverUntil(helpText, keyDown("h"))),
  keyDown("p").andThen(tutorial),
  clickOn(ClickType.Double, MouseButton.Left, S)
    .flatMap(editStringAttr(S, SName)),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
  clickOn(ClickType.Double, MouseButton.Left, T)
    .flatMap(editStringAttr(T, TName)),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
)

val initBindings = Bindings[PropPetri, Unit](
  keyDown("Escape").andThen(
    for {
      _ <- hideTip
      _ <- bindings.runForever
    } yield ()),
  keyDown("p").andThen(tutorial).andThen(bindings.runForever)
)

val L = actionLiftIO[PropPetri]

def arcExtractor(p: PropPetri, sprites: Sprites) = {
  val bends = assignBends(List((I, IS, IT, 1), (O, OS, OT, -1)), p, 0.3)
  val inputs = edgeExtractor(IS, IT)(p, sprites, bends)
  val outputs = edgeExtractor(OT, OS)(p, sprites, bends)
  inputs ++ outputs
}

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
                s.subpart(Props(S), v).get + (Content, s.subpart(SName, v).getOrElse(""))
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#6C9AC3")
              + (Stroke, "none")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "#97b7d4")),
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
                  + (Content, s .subpart(TName, v).getOrElse(""))
                  + (Pulse, s.subpart(Rate, v).get)
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#E28F41")
              + (Stroke, "none")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "#eaaf78")),
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

val serializer = withPropsACSet[LabelledReactionNet].rw(
  AttrTypeSerializers()
    + ATRW(PropValue, PropMap.rw)
    + ATRW(NameValue, summon[ReadWriter[String]])
    + ATRW(RateValue, summon[ReadWriter[Double]])
    + ATRW(ConcentrationValue, summon[ReadWriter[Double]])
)

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action: M[Unit] = for {
      $model <- ReaderT.ask.map(_.$model)
      hover <- ReaderT.ask.map(_.hover)
      drag <- ReaderT.ask.map(_.drag)
      mouse <- ReaderT.ask.map(_.mouse)
      _ <- addChild(renderPetri($model, hover, drag, mouse))
      _ <- showTip(
        "Click on this box, and hit p to play the tutorial!",
        "Or if you're already a pro, hit escape to get to the normal editor"
      )
      _ <- initBindings.run
    } yield ()

    mountWithAction(el, WithProps[LabelledReactionNet](), serializer, action)
  }
}

package semagrams.ui

import semagrams._
import semagrams.acsets._
import semagrams.sprites.GenericHTMLSprite
import semagrams.util._
import com.raquo.laminar.api.L._

import cats.effect._

/** An entity that is created in an ad-hoc manner to track some arbitrary bit of
  * UI state
  */
case class AnonEntity(token: Unique.Token) extends Entity {
  val ty = AnonEntity
}

/** The type of [[AnonEntity]] */
object AnonEntity extends EntityType

/** Construct a new unique [[AnonEntity]] */
val newEntity: IO[Entity] = for {
  t <- IO.unique
} yield AnonEntity(t)

/** State for managing a collection of UI elements
  *
  * @param sprites
  *   A collection of UI sprites, i.e. dialogues, text boxes, buttons, etc.
  *
  * @param focus
  *   A function which focuses the global state (doesn't currently work exactly
  *   as intended I think).
  *
  * @param globalSize
  *   A Signal whose value is the size of the overall window. Used for
  *   positioning
  */
case class UIState(
    sprites: Var[Vector[(Entity, Sprite, ACSet)]],
    focus: () => Unit,
    globalSize: Signal[Complex]
) {

  /** Add a new entity with a sprite and ACSet, or update a pre-existing entity
    * if it exists
    */
  def addEntity(e: Entity, s: Sprite, p: ACSet): IO[Unit] =
    IO(sprites.update(entities => {
      val i = entities.indexWhere(_._1 == e)
      if (i != -1) {
        entities.updated(i, (e, s, p))
      } else {
        entities :+ (e, s, p)
      }
    }))

  /** Add a new entity that wraps an HtmlElement using [[GenericHTMLSprite]] */
  def addHtmlEntity(e: Entity, build: () => HtmlElement): IO[Unit] =
    addEntity(e, GenericHTMLSprite(build, globalSize), ACSet(SchEmpty))

  /** Add a new entity where the builder function takes in a callback that
    * allows it to delete itself.
    */
  def addKillableHtmlEntity(
      e: Entity,
      build: Observer[Unit] => HtmlElement
  ): IO[Unit] =
    addEntity(
      e,
      GenericHTMLSprite(() => build(Observer(_ => remEntity(e))), globalSize),
      ACSet(SchEmpty)
    )

  /** Same as [[addKillableHtmlEntity]], but with a fresh [[AnonEntity]] */
  def addKillableHtmlEntity(build: Observer[Unit] => HtmlElement): IO[Unit] =
    for {
      e <- newEntity
      _ <- addKillableHtmlEntity(e, build)
    } yield ()

  /** Same as [[addKillableHtmlEntity]], but the callback allows you to actually
    * return something. Creates the entity, waits asynchronously for the
    * callback to be called, and then kills the entity and returns what was
    * passed to the callback.
    */
  def dialogue[A](build: Observer[A] => HtmlElement): IO[A] = for {
    e <- newEntity
    // I don't know why this is necessary
    _ <- IO.print("")
    a <- IO.async[A](cb =>
      addEntity(
        e,
        GenericHTMLSprite(() =>
          build(Observer(a => {
            cb(Right(a))
            remEntity(e)
            focus()
          })),
          globalSize
        ),
        ACSet(SchEmpty)
      ).flatMap(_ => IO(None))
    )
  } yield a

  /** Removes the entity `e` */
  def remEntity(e: Entity) = {
    sprites.update(_.filterNot(_._1 == e))
  }

  /** The viewport that should be added to the main window.
    *
    * Maybe instead, [[UIState]] should implement [[Viewport]]?
    */
  val viewport = EntitySourceViewport(
    sprites.signal,
    Seq(EntitySource((entities, _) => entities))
  )
}

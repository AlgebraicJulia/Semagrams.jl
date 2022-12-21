package semagrams.ui

import semagrams._
import semagrams.sprites.GenericHTMLSprite
import semagrams.util._
import com.raquo.laminar.api.L._

import cats.effect._

case class AnonEntity(token: Unique.Token) extends Entity

val newEntity: IO[Entity] = for {
  t <- IO.unique
} yield AnonEntity(t)

case class UIState(
  sprites: Var[Vector[(Entity, Sprite, PropMap)]],
  globalSize: Signal[Complex]
) {
  def addEntity(e: Entity, s: Sprite, p: PropMap) =
    IO(sprites.update(
      entities => {
        val i = entities.indexWhere(_._1 == e)
        if ( i != -1 ) {
          entities.updated(i, (e, s, p))
        } else {
          entities :+ (e, s, p)
        }
      }
    ))

  def addHtmlEntity(e: Entity, build: () => HtmlElement) =
    addEntity(e, GenericHTMLSprite(build, globalSize), PropMap())


  def addKillableHtmlEntity(e: Entity, build: Observer[Unit] => HtmlElement): IO[Unit] =
    addEntity(e, GenericHTMLSprite(() => build(Observer(_ => remEntity(e))), globalSize), PropMap())


  def addKillableHtmlEntity(build: Observer[Unit] => HtmlElement): IO[Unit] = for {
    e <- newEntity
    _ <- addKillableHtmlEntity(e, build)
  } yield ()

  def remEntity(e: Entity) = {
    sprites.update(_.filterNot(_._1 == e))
  }

  val viewport = EntitySourceViewport(
    sprites.signal,
    Seq(EntitySource((entities,_) => entities))
  )
}

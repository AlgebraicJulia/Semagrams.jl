package semagrams

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.ui._
import semagrams.util._
import semagrams.widgets._
import monocle._

import com.raquo.laminar.api.L._
import cats.effect._
import semagrams.controllers.HoverController
import semagrams.acsets.WiringDiagrams.OutPort
import semagrams.acsets.WiringDiagrams.InPort
import semagrams.sprites.Middleware
import semagrams.sprites.WithMiddleware
import semagrams.sprites.DPBox
import semagrams.sprites.Rect

case class Actions(
  es: EditorState,
  m: UndoableVar[ACSet],
  ui: UIState,
  serialize: ACSet => String,
  deserialize: String => Option[ACSet],
) {
  import ACSet._




    // // PrintWriter
    // import java.io._
    // val pw = new PrintWriter(new File("hello.txt" ))
    // pw.write("Hello, world")
    // pw.close

    // // FileWriter
    // val file = new File(canonicalFilename)
    // val bw = new BufferedWriter(new FileWriter(file))
    // bw.write(text)
    // bw.close()

    



  def addAtMouse(ob: Ob, init: ACSet) = for {
    pos <- es.mousePos
    x <- m.updateS(addPart(ob, init.setSubpart(ROOT, Center, pos)))
  } yield x

  def addAtMouse(ob: Ob) = for {
    pos <- es.mousePos
    x <- m.updateS(addPart(ob, PropMap() + (Center, pos)))
    // _ = println(x)
  } yield x

  def addAtMouse_(ob: Ob, init: ACSet) = addAtMouse(ob,init).map(_ => ())

  def addAtMouse_(ob:Ob) = addAtMouse(ob).map(_ => ())

  def add(p: Part, ob: Ob, props: PropMap) = m.updateS(
    addPart(p, ob, props)
  )

  def add_(p: Part, ob: Ob, props: PropMap) = add(p,ob,props).map(_ => ())

  def set(p: Part, f: Property, v: f.Value) = m.updateS(
    setSubpart(p,f,v)
  )

  def remove(p:Part,f:Property) = m.updateS(
    remSubpart(p,f)
  )

  def remove(p:Part) = m.updateS(
    remPart(p)
  )

  val del = for {
    ment <- es.hovered
    _ <- ment match {
      case Some(ent: Part) => m.updateS_(remPart(ent))
      case _               => IO(())
    }
    _ = es.hover.
    
    $state.set(HoverController.State(None))
  } yield ()


  def getBBox(b:Part): IO[BoundingBox] = b.path match 
    case Seq() => 
      val sz = es.size.now()
      IO(BoundingBox(sz/2.0,sz))
    case Seq(head) => for {
      sprtry <- IO(es.entities.now().em.get(b))
      sprdata <- sprtry match
        case Some(pair) => fromMaybe(IO(sprtry))
        case None => 
          println(s"bad $b")
          die
      (spr,acset) = sprdata
      bbtry = spr.bbox(Part(Nil),acset)
      bb <- {bbtry match
        case Some(bb) => IO(bb)
        case None =>
          println(s"bad bb: $b")
          die
      }
    } yield bb
    case Seq(head,rest @_*) => for {
      bdata <- fromMaybe(IO(es.entities.now().em.get(b.head)))
      (spr,acset) = bdata
      _ = println(spr.bbox(Part(rest),acset))
      bb <- getBBox(ROOT)
    } yield bb
    
  //   fromMaybe(IO(
  //   es.entities.now().em.get(b).map(pr => pr._1 match
  //     case dp:DPBox => dp.boxSprite match
  //       case mw:WithMiddleware => mw.s.bbox(Part(Nil),pr._2)
  //       case s => println(s"!: $s")
  //     case t => println(s"?: $t")
  // )))
    

  // def subspriteInfo(b:Part): Option[ACSet] = es.entities.now().em.get(b).map(_._2)

  // def drag(i: Part) = for {
  //   _ <- IO(m.save())
  //   _ <- IO(m.unrecord())
  //   _ <- m.updateS_(moveFront(i))
  //   c <- IO(m.now().subpart(Center, i))
  //   init <- es.mousePos
  //   offset <- IO(c - init)
  //   _ <- es.drag.drag(
  //     Observer(p => { m.update(_.setSubpart(i, Center, p + offset)) })
  //   )
  //   _ <- IO(m.record())
  // } yield ()



  def drag[Memo,Return](
      start: (z:Complex) => IO[Memo],
      during: (Memo,Complex) => Unit,
      end: Memo => IO[Return]
    ): Part => IO[Return] = (s:Part) => for {
      _ <- IO(m.save())
      _ <- IO(m.unrecord())
      m0 = m.now()
      memo <- es.mousePos.flatMap(start)
      ret <- (for {
        _ <- es.drag.drag(Observer(p => during(memo,p)))
        ret0 <- end(memo)
      } yield ret0).onCancelOrError(for
        _ <- IO(es.drag.$state.set(None))
        _ <- IO(m.set(m0))
      yield ())
      _ <- IO(m.record())
    } yield ret.asInstanceOf[Return]





  // Memo = Offset, Return = Unit
  def dragMove(i: Part) = 
    def start = (z:Complex) => for 
      _ <- m.updateS_(moveFront(i))
      c <- IO(m.now().subpart(Center, i))
    yield c - z

    def during = (offset:Complex,p:Complex) => m.update(
      _.setSubpart(i, Center, p + offset)
    )

    def end = (offset:Complex) => IO(())

    drag(start,during,end)(i)



  val debug = IO(m.now()).flatMap(IO.println)
  def die[A]: IO[A] = fromMaybe(IO(None))
  

  // Memo = Return = wire
  // def makeWire(w:Ob,src:Hom,tgt:Hom) = (s:Part) => {

  //   def start = () => for 

  //   yield 
  // }

  def liftTo(p:Part,tp:PartType): IO[Part] = 
    // println(s"liftTo: $p, $tp")
    tp.path match
    case Seq() => 
      // println(s"lifted to $p")
      IO(p)
    case Seq(next,rest @_*) => {for
      ext <- add(p,next,PropMap())
      // _ = println(s"added $ext")
      last <- liftTo(ext,PartType(rest))
    yield last}
        

  // def liftTo(p:Part,tps: Seq[PartType]): IO[Part] = 
  //   println(s"liftTos: $p, $tps")
  //   tps match
  //   case Seq() => 
  //     println(s"die $p, $tps")
  //     die
  //   case Seq(tp,rest @_*) => 
  //     println(s"trying lift $p to $tp")
  //     liftTo(p,tp).onCancelOrError(liftTo(p,rest))

  def noLift(p:Part,pos:Complex): PartType = ROOT.ty



  
  // Memo = Part, Return = Part
  def dragEdge(
    ob:Ob,
    src:Hom,
    tgt:Hom,
    liftSeq: (Part,Complex) => PartType = noLift
  )(s: Part): IO[Part] =

      

    def start = (z:Complex) => 
      val ptype = liftSeq(s,z)
      // println(s"ptype: $ptype")
      for
        p <- liftTo(s,ptype)
        // _ = println(s"p = $p")
        w <- add(
          ROOT,
          ob,
          p.ty match
            case tp if src.codoms.contains(tp) =>
              PropMap().set(src,p)
                .set(End, z).set(Interactable, false)
            case tp if tgt.codoms.contains(tp) => 
              PropMap().set(tgt,p)
                .set(Start, z).set(Interactable, false)
        )
      yield w

    def during = (e:Part,p:Complex) => if m.now().hasSubpart(src,e)
    then m.update(_.setSubpart(e, End, p))
    else m.update(_.setSubpart(e, Start, p))

    def end = (e:Part) => for
      t <- fromMaybe(es.hoveredPart)
      // _ = println(s"end: $t")
      z <- es.mousePos
      qtype = liftSeq(t,z)
      q <- liftTo(t,qtype)
      _ <- m.now() match
        case mnow if mnow.hasSubpart(src,e) => for {
          _ <- set(e, tgt, q)
          _ <- remove(e, End)
          _ <- remove(e, Interactable)
        } yield ()
        case mnow if mnow.hasSubpart(tgt,e) => for {
          _ <- set(e,src,q)
          _ <- remove(e,Start)
          _ <- remove(e,Interactable)
        } yield ()
        case _ => die
    yield e

    drag(start,during,end)(s)

  


  // def killDrag(m0:ACSet) = for
  // yield ()

  def edit(p: Property { type Value = String; }, multiline: Boolean)(i: Part): IO[Unit] = for {
    _ <- IO(m.update(acs => if (acs.trySubpart(p, i).isEmpty) {
                       acs.setSubpart(i, p, "")
                     } else {
                       acs
                     }
            ))
    _ <- ui.addKillableHtmlEntity(
      kill => {
        val v = m.zoomL(subpartLens(p, i))
        val t = TextInput(v, multiline)(kill)
        if (multiline) {
          PositionWrapper(Position.topToBotMid(10), t)
        } else {
          PositionWrapper(Position.botMid(10), t)
        }
      }
    )
  } yield ()



  def importExport = ui.addKillableHtmlEntity(
    kill => PositionWrapper(
      Position.topToBotMid(10),
      TextInput(m.zoomL(Lens(serialize)(s => a => deserialize(s).getOrElse(a))), true)(kill)
    )
  )

  
}

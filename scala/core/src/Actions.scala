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
import cats.data.State

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

    
  def bgACSet() = m.now().subacset(es.bgPart())



  def addAtMouse(ob: Ob, init: ACSet): IO[Part] = for {
    pos <- es.mousePos
    x <- m.updateS(State(_.subacset(es.bgPart())
      .addPart(
        es.bgPart(),
        ob,
        init.setSubpart(ROOT,Center,pos)
      )
    ))
  } yield x

  def addAtMouse(ob: Ob): IO[Part] = for {
    pos <- es.mousePos
    x <- m.updateS(State(
      _.addPart(
        es.bgPart(),
        ob,
        PropMap() + (Center, pos)
      )
    ))
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


  def getBBox(b:Part): BoundingBox =
    val b_ent = es.bgPart() match
      case ROOT => b
      case bg => bg.last
    b match 
      case b if b == es.bgPart() =>
        // println("case 1")
        val sz = es.size.now()
        BoundingBox(sz/2.0,sz)
      case b if es.bgPlus(b.last) == b => 
        println("case 2")
        
        val (spr,acset) = es.entities.now().em.get(b_ent)
          .getOrElse({
            println(s"getBBox: missing entity $b")
            throw new RuntimeException(s"getBBox: missing entity $b")
          })

        // println("got here")
        // println(acset.partsMap(b.ty.path(0)).acsets.keys)
        val sub = es.bgPart() match
          case ROOT => acset
          case _ => acset.subacset(b.last)
        // println(s"sub = $sub")
        val bb = spr.bbox(Part(Nil),sub).getOrElse({
          println(s"getBBox: missing entity $b")
          throw new RuntimeException(s"getBBox: missing bbox $b")
        })
        // println("and here")
        // println(s"bb = $bb")
        bb
      case b if es.bgPart() < b => 
        println("case 3")
        // println(es.entities.now().em.keys)
        val (spr,acset) = es.entities.now().em.get(b_ent)
          .getOrElse({
            println(s"getBBox: missing entity $b")
            throw new RuntimeException(s"getBBox: missing entity $b")
          })
        val bb = spr.bbox(ROOT,acset.subacset(b.tail)).getOrElse({
          println(s"getBBox: missing bbox $b")
          throw new RuntimeException(s"getBBox: missing bbox $b")
        })
        bb
      case _ => 
        println(s"case 4: $b, ${es.bgPart()}, ${es.bgPart() > b}, ${es.bgPart() < b}")
        throw new RuntimeException(s"bad getBBox: $b, ${es.bgPart()}")
        
  
  def getCenter(p:Part) = getBBox(p).pos
  def getSize(p:Part) = getBBox(p).dims
    
    
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

    val pt = es.currentView.now().extend(i).asInstanceOf[Part]

    def start = (z:Complex) => for 
      _ <- m.updateS_(moveFront(pt))
      c <- IO(m.now().subpart(Center, pt))
    yield c - z

    def during = (offset:Complex,p:Complex) => m.update(
      _.setSubpart(pt, Center, p + offset)
    )

    def end = (offset:Complex) => IO(())

    drag(start,during,end)(pt)



  val debug = IO(m.now()).flatMap(IO.println)
  def die[A]: IO[A] = fromMaybe(IO(None))
  

  // Memo = Return = wire
  // def makeWire(w:Ob,src:Hom,tgt:Hom) = (s:Part) => {

  //   def start = () => for 

  //   yield 
  // }

  def liftTo(p:Part,ob:Ob,idx:Int): IO[Part] = for
    pt <- add(p,ob,PropMap())
    _ = m.update(acset => acset.moveToIndex(pt,idx))
  yield pt
         
    
  //   tp.path match
  //   case Seq() => IO(p)
  //   case Seq(next,rest @_*) => {for 
      
  //     // _ = println(s"added $first")      
  //     last <- liftTo(first,PartType(rest),idx)
  //     // _ = println(s"finished $last")
  //   yield last}
  
    

  // Memo = Part, Return = Part
  def dragEdge(
    ob:Ob,
    src:Hom,
    tgt:Hom,
    lift: (Part,Complex) => Option[(Ob,Int)] = (_,_) => None
  )(s: Part): IO[Part] =
    // println(s"dragEdge $s")
      
    // val s = bgPlus(s_ext)
    def start = (z:Complex) =>
      // println(s"start: s = $s")
      // println(s"ptype = $ptype")
      
      for
        p <- {lift(s,z) match
          case Some((ptype,idx)) => liftTo(s,ptype,idx)
          case None => IO(s)
        }
        // _ = println(s"p = $p")
        // _ = println(p - es.bgPart())

        w <- add(
          es.bgPart(),
          ob,
          p - es.bgPart() match
            case pt if src.codoms.contains(pt.ty) =>
              // println(s"if1 $pt")
              PropMap().set(src,pt)
                .set(End, z).set(Interactable, false)
            case pt if tgt.codoms.contains(pt.ty) => 
              // println(s"if2 $pt")
              PropMap().set(tgt,pt)
                .set(Start, z).set(Interactable, false)
            case tp => 
              // println(s"unmatched $tp")
              // println(src.codoms)
              PropMap().set(Start,z).set(End,z).set(Interactable,false)
        )
        // _ = println(w)
      yield w

    def during = (e:Part,p:Complex) => 
      // println(s"during $e")
      if m.now().hasSubpart(src,e)
      then m.update(_.setSubpart(e, End, p))
      else m.update(_.setSubpart(e, Start, p))

    def end = (e:Part) => for
      t <- fromMaybe(es.hoveredPart)
      // _ = println(s"end: $t")
      z <- es.mousePos
      q <- lift(t,z) match
        case Some((qtype,idx)) => liftTo(t,qtype,idx)
        case None => IO(t)
      // _ = println(s"q = $q, e = $e")
      _ <- m.now() match
        case mnow if mnow.hasSubpart(src,e) => for {
          _ <- set(e, tgt, q - es.bgPart())
          // _ = println("end1")
          _ <- remove(e, End)
          _ <- remove(e, Interactable)
        } yield ()
        case mnow if mnow.hasSubpart(tgt,e) => for {
          _ <- set(e,src,q - es.bgPart())
          // _ = println("end2")
          _ <- remove(e,Start)
          _ <- remove(e,Interactable)
        } yield ()
        case _ => 
          // println(s"end3 $e")         
          die
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

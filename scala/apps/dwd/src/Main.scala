package semagrams.dwd

import semagrams.Viewport
import semagrams.api._
import semagrams.dblClickOn
import semagrams.acsets.{_, given}

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._
import semagrams.Background
import semagrams.EntityCollection


import scala.language.implicitConversions
import scala.reflect.ClassTag
import semagrams.sprites.{AltDPBox,BasicPort}
import semagrams.util.msgError
import semagrams.sprites.DPBox


case object SchDWD extends Schema {

  val obs = Seq(DWDObs.values*)
  val homs = Seq(DWDHoms.values*)
  val attrs = Seq()



  enum DWDObs(_schema: Schema = SchEmpty) extends Ob:    
    override lazy val schema = _schema
    case InPort, OutPort, Wire
    case Box extends DWDObs(SchDWD)

  import DWDObs._

  // case object SchBox extends Schema {
  //   val obs = Seq(InPort,OutPort)
  //   val homs = Seq()
  //   val attrs = Seq()
  // }




  enum DWDHoms(val doms:Seq[PartType],val codoms:Seq[PartType]) extends Hom:
    case Src extends DWDHoms(
      Wire.asDom(),
      InPort.asDom() :+ Box.extend(OutPort)
    )
    case Tgt extends DWDHoms(
      Wire.asDom(),
      OutPort.asDom() :+ Box.extend(InPort)
    )

  
  
}

import SchDWD._
import DWDObs._
import DWDHoms._


object DWD {
  def apply() = ACSet(SchDWD)
  def apply(init:ACSet) = new ACSet(SchDWD,init.props,init.partsMap)
}










def bindings(
  es: EditorState, 
  g: UndoableVar[ACSet], 
  ui: UIState,
  vp: Viewport
) = {

  val a = Actions(es,g,ui)



  def test_ser(p:Part): IO[Unit] = IO({
    println(s"testing serialization for $p")
    val ps = g.now().subacset(p)
    println(s"initial acset: $ps")
    val s = write(ps)
    println(s"serialized: $s")
    val a = read[ACSet](s)
    println(s"deserialized: $a")
    println(s"deserialized == init? ${ps == a}")
  })

  val boxMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
    ("Add Input", (b:Part) => IO(println("addIn"))),
  )

  val wireMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
  )


  def portNumber(pos:Complex,size:Complex,nports:Int) =
    val l = (0 to nports+1).map(
      _ * size.y/(nports + 1)
    )
    // println(s"l = $l")
    // println(s"pos = $pos")
    val hit = l.lastIndexWhere(_ < pos.y)
    // println(l.map(_ < pos.y))
    // println(hit)
    hit


  def portByPos(p: Part, pos: Complex): Seq[(Ob,Int)] =
    val obseq = (p - es.bgPart).ty match
      case ROOT.ty =>
        val sz = es.size.now()
        if pos.x < (sz.x / 2.0)
        then Seq((InPort,1))
        else Seq((OutPort,1))
      case PartType(Seq(Box)) =>
        val ctr = g.now().trySubpart(Center, p)
        println(s"pos = $pos, ctr = $ctr")
        ctr match
          case None => Seq((InPort,1))
          case Some(c) =>
            if pos.x < c.x
            then Seq((InPort,1))
            else Seq((OutPort,1))
      case _ => 
        println(s"portByPos unknown part $p")
        Seq()
    obseq


  import MouseButton._
  import KeyModifier._



  def test(p:Part) = es.mousePos.map(z => 
    println(portByPos(p,z))  
    
  )

  // def portByPos(p:Part,pos:Complex): Option[(Ob,Int)] = p match
  //   case b if b == es.bgPart =>
  //     // println(s"portByPos bg $b")
  //     val sz = es.size.now()
  //     val ptype = if pos.x < (sz.x / 2.0)
  //       then InPort
  //       else OutPort
  //     val nports = g.now().subacset(b).partsMap(ptype).ids.length
  //     Some((ptype,portNumber(pos,sz,nports)))
  //   case p if Seq(InPort,OutPort).contains(p.ty.path.last) => None 
  //   case b if b.init == es.bgPart =>
  //     println(s"portByPos box $b")
  //     val ctr = g.now().subacset(es.bgPart).trySubpart(Center,p-es.bgPart).getOrElse({
  //       println("missing center")
  //       throw new RuntimeException("broke")
  //     })
  //     val ptype = if pos.x < ctr.x then InPort else OutPort
  //     val nports = g.now().subacset(b).partsMap(ptype).ids.length
  //     // val sz = a.getSize(b)
  //     // println(s"sz = $sz")
  //     // println(s"x: ${ctr.x > pos.x}")
  //     // println(s"y: ${ctr.y > pos.y}")
  //     Some((ptype,nports))
  //   case _ => 
  //     // println(s"unknown portByPos $p")
  //     None
  

  // def boxSize(b:Part): Option[Complex] = 
  


  def zoomIn(b:Part) =  
    // println(s"zoomIn $b")
    // val spr = es.getSprite(b)

    es.hover.$state.set(HoverController.State(None))
    es.currentView.set(b)
    es.deregister("mainVP")
    for 
      // bb <- a.getBBox(b)
      // _ = println(bb)
      vp <- es.makeViewport(
        "mainVP",
        es.size.signal.combineWith(g.signal.map(_.subacset(b)))
          .map((sz,acset) => DPBox.layoutPorts(InPort,OutPort)(BoundingBox(sz/2.0,sz),acset)),
        entitySources(es)
      )
      // .map(vp => 
      //   es.viewports.set(Set(vp))
      // )
      _ = println("done")
    yield vp

  def zoomOut = 
    val b = es.bgPart match
      case ROOT => ROOT
      case p => p.init
    zoomIn(ROOT).flatMap(_ => zoomIn(b))


  
  Seq(
    keyDown("t").andThen(fromMaybe(es.hoveredPart).flatMap(test)),
    // clickOn(Left).map(ent => println(s"click ent = $ent")),
    // clickOnPart(Left).map(pt => println(s"click pt = $pt")),
    // Add box
    dblClickOnPart(Left,ROOT.ty).withMods().flatMap(
      _ => for
        b <- a.addAtMouse(Box)
        // _ <- a.set(b,Content,s"${util.Random.alphanumeric.take(3).mkString}")
        _ <- a.edit(Content,true)(b)
      yield ()
    ),
    dblClickOnPart(Left,PartType(Seq(Box)))
      .withMods()
      .map(b => es.bgPlus(b))
      .flatMap(a.edit(Content,true)),
    dblClickOnPart(Left,PartType(Seq(Box)))
      .withMods(Ctrl)
      .map(b => es.bgPlus(b))
      .flatMap(zoomIn),

    dblClickOn(Left).withMods(Ctrl).flatMap(_ => zoomOut),
    clickOnPart(Left).withMods()
      .flatMap(p => 
        p.ty match
        case ROOT.ty => es.mousePos.flatMap(
          z => IO(())//IO(println(s"clicked at $z"))
        )
        case PartType(Seq(Box,_*)) => a.dragMove(p.head)
      ),
    clickOnPart(Left).withMods(Shift)
      .map(es.bgPlus(_))
      .flatMap(p => for
        e <- a.dragEdge(Wire,Src,Tgt,portByPos)(p)
        _ <- a.edit(Content,true)(e)
      yield ()),
    menuOnPart().flatMap(p => 
      println(s"menuOnPart $p")
      p match
      case w if w.lastOb == Wire => 
        es.makeMenu(ui,wireMenu)(p)
      case b if b.lastOb == Box =>
        es.makeMenu(ui,boxMenu)(p)
      case _ => a.die
    ),
    keyDown("d").andThen(a.del),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    // to remove
    keyDown("?").andThen(a.debug),
    keyDown("s").andThen(a.importExport)
  )
}



  


// def layoutPorts(dims: Complex, init: ACSet): ACSet = {
//   import Complex.im
//   def helper(acs: ACSet, portOb: Ob, dir: (-1) | 1): ACSet = {
//     val ports = acs.parts(ROOT, portOb)
//     val sideCenter = dims / 2 + (dir * dims.x / 2)
//     val spacer = FixedRangeExceptEnds(-dims.y / 2, dims.y / 2)
//     val n = ports.length
//     val cs = ports.zipWithIndex.map(
//       {
//         case ((p, sub), i) => (p, sideCenter + spacer.assignPos(i,n) * im)
//       }
//     )
//     cs.foldLeft(acs)((acs, pc) => acs.setSubpart(pc._1, Center, pc._2))
//   }
//   helper(helper(init, InPort, -1), OutPort, 1)
// }






def wireProps(
    src: Hom,
    tgt: Hom,
    bg: Part
)(_e: Entity, acs: ACSet, m: EntityMap): PropMap = {
  
  val p = acs.props

  val s = p.get(src)
  val t = p.get(tgt)


  // println(s"bg = $bg")
  // println(s.map(_ - bg))

  val tspos = s.flatMap(pt => findCenter((pt - bg),m))
  val ttpos = t.flatMap(pt => findCenter((pt - bg),m))
  
  // println(s"wireProps m = ${m.keys}")
  // println(s"""wireProps
  //   s = $s
  //   tspos = $tspos
  //   t = $t
  //   ttpos = $ttpos
  //   _e = $_e
  //   acs = $acs
  //   m = ${m.keys}


  //   """)

  // println(s"tspos = $tspos, ttpos = $ttpos")

  val spos = tspos match
    case Some(z) => z
    case None => 
      // println(s"no spos")
      p.get(Start).getOrElse({
        // println(s"missing Start $_e")
        Complex(100,200)
      })

  val tpos = ttpos match
    case Some(z) => z
    case None => 
      // println(s"no tpos")
      p.get(End).getOrElse({
        // println(s"missing End $_e")
        Complex(100,100)
      })

  
  def getDir(s:Option[Part]) = s.map(_ - bg) match
    case Some(p) 
      if p.ty.path == Seq(Box,OutPort) |
        p.ty.path == Seq(InPort)
      => 10.0
    case Some(p) 
      if p.ty.path == Seq(Box,InPort) |
        p.ty.path == Seq(OutPort)
      => -10.0
    case _ => 0.0

  val (startDir,endDir) = (getDir(s),getDir(t))

  PropMap().set(WireProp.StartDir,startDir).set(WireProp.EndDir,endDir)
    + (Start, spos) + (End, tpos) + (Stroke,"black")
    
}



def ACSetWireSource(
    ob: Ob,
    src: Hom,
    tgt: Hom,
    sprite: Sprite,
    es: EditorState
) = ACSetEntitySource(ob, sprite).addPropsBy(wireProps(src, tgt, es.bgPart))

val entitySources = (es:EditorState) => Seq(
  ACSetEntitySource(Box, AltDPBox(InPort, OutPort)(es)).withProps(PropMap() + (FontSize,22)),
  ACSetEntitySource(InPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightblue")),
  ACSetEntitySource(OutPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightblue")),
  ACSetWireSource(Wire, Src, Tgt, BasicWire(es),es),
)






object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = DWD()
      // val spr = entitySources(es).head.sprite
      // val initg = proc
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal)
            .map(DPBox.layoutPortsBg(InPort,OutPort))
        )
        vp <- es.makeViewport(
          "mainVP",
          lg,
          entitySources(es)
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui,vp))
      } yield ()
    }
  }
}

package semagrams.mwe

import com.raquo.laminar.api.L.{_, given}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel

import semagrams._
import semagrams.acsets.{_, given}
import semagrams.actions._
import semagrams.controllers._
import semagrams.sprites._
import semagrams.util._


// ================= Schemas ==================

// Internals (objects, arrows & attributes)
case object Node extends Ob
case object Edge extends Ob
case object src extends HomWithDom {
  val dom = Edge
  val codom = Node
}
case object tgt extends HomWithDom {
  val dom = Edge
  val codom = Node
}
case object label extends AttrWithDom with PValue[String] {
  val dom = Node
}

// Externals (schema object & type)

case object GraphSchemaOb extends StaticSchema {
  val schema = BasicSchema(
    Node, Edge, src, tgt, label
  )
}
type GraphSchema = GraphSchemaOb.type

// ================= ACSets ===================

def buildGraph() = {
  
  // Constructors
  val g0 = Graph()
  val (g1,n1) = g0.addPart(Node)
  val g2 = g1.setSubpart(label,n1,"My node")
  val (g3,n2) = g2.addPart(
    Node,
    PropMap().set(label,"My other node")
  )

  val (g4,e) = g3.addPart(
    Edge,
    PropMap().set(src,n2).set(tgt,n1)
  )
  
  val (g5,_) = g4.addPart(
    Edge,
    PropMap().set(src,n1).set(tgt,n2)
  )

  // Accessors
  println(g4.parts(Node))
    // == Set(Part(0,Node),Part(1,Node))
  println(g4.parts(Edge))
    // == Set(Part(2,Edge))
  println(g4.props(n1))
    // == Map(label -> "My node")
  println(g4.props(e))
    // == Map(src -> Part(1,Node),tgt -> Part(0,Node))
  
  g5
}

// ========= Graphs (type alias + extensions) =============
type Graph = ACSet[GraphSchema]
  extension (g:Graph) {
    def nodes: Set[Part] = g.parts(Node)
    def edges: Set[Part] = g.parts(Edge)
    
    def srcOf(e: Part): Option[Part] = g.trySubpart(src,e).asInstanceOf[Option[Part]]
    def tgtOf(e: Part): Option[Part] = g.trySubpart(tgt,e)
    def labelOf(n: Part): Option[String] = g.trySubpart(label,n)

    def edgesIn(n: Part) = g.incident(tgt,Edge,n)
    def edgesOut(n: Part) = g.incident(src,Edge,n)

    def edgesFrom(s: Part,t: Part): Set[Part] = 
      edgesOut(s) intersect edgesIn(t)
  }


object Graph {
  def apply() = ACSet[GraphSchema]()
}

//================== Interaction ========================

// Actions

val ops = Action.ops[Graph]
val aops = summon[ACSetOps[GraphSchema]]





def initialize(g: Graph): Action[Graph,Unit] = for
  _ <- ops.ask.flatMap(_ => updateModel(_ => g))
  ns <- ops.ask.map(_.$model.now().parts(Node).toSeq)
  zs <- randPts(ns.length)
  _ <- ops.ask.flatMap(
    _ => setSubparts(Center,ns.zip(zs))
  )
yield ()
  


def addNode(props:PropMap = PropMap()): Action[Graph,Part] = for
  p <- ops.ask.flatMap(_ => mousePos)
  n <- ops.ask.flatMap(
    _ => addPart(
      Node,
      PropMap() + (Center,p)
    )
  )
yield n
  


def addEdge(s: Part,t: Part): Action[Graph,Part] = 
  ops.ask.flatMap(_ => addPart(
    Edge,
    PropMap().set(src,s).set(tgt,t)
  ))

def dragGraphEdge(s:Part): Action[Graph,Part] = for
  drag <- ops.ask.map(_.drag)
  $m <- ops.ask.map(_.$model)
  p <- mousePos
  e <- updateModelS(
    aops.addPart(Edge,
      PropMap().set(src,s).set(Start,p).set(End,p)
    )
  )
  _ <- (for 
    _ <- drag.drag(
      Observer(p => $m.update(
        _.setSubpart(End,e,p)
      ))
    )
    t <- fromMaybe(hoveredPart(Node))
    _ <- updateModelS(
      aops.setSubpart(tgt,e,t)
    )
    _ <- updateModelS(
      aops.remSubpart(End,e)
    )
    _ <- updateModelS(
      aops.remSubpart(Start,e)
    )
  yield ()).onCancelOrError(for
    _ <- ops.delay(drag.$state.set(None))
    _ <- updateModelS(aops.remPart(e))
  yield ())
  _ <- update
yield e


def setLabel(n: Part): Action[Graph,Unit] = editStringProp(label)(n)

// Bindings



val bindings = Bindings[Graph, Unit](
  clickOn(ClickType.Double,MouseButton.Left,BackgroundType)
    .flatMap(_ => addNode())
    .map(_ => ()),
  clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
    .flatMap(_ => dragPan),

  clickOnPart(ClickType.Double,MouseButton.Left)
    .flatMap(setLabel),
  clickOnPart(ClickType.Single,MouseButton.Left)
    .flatMap(dragPart),
  clickOnPart(ClickType.Single,MouseButton.Left)
    .withMods(KeyModifier.Shift)
    .flatMap(s => dragGraphEdge(s))
    .map(_ => ()),

  keyDown("Delete").andThen(remPart)
)



// =========== Rendering =======================

// Entity & Prop extraction

def extractNodes(g: Graph): List[(Part,PropMap)] = g.nodes.map(
  n => (n, g.props(n)
    + (Content, g.labelOf(n).getOrElse("?"))
  )
).toList




def extractEdges(g: Graph, sprites: Sprites): List[(Part,PropMap)] = g.edges.toList.map(e =>
  val pts = for
    s <- g.trySubpart(src,e)
    t <- g.trySubpart(tgt,e)
  yield (s,t)
  pts match
    case Some((s,t)) => 
      if s == t 
      then (e,loopProps(g,e,sprites))
      else (e,edgeProps(g,e,sprites))
    case None => (e,dragProps(g,e,sprites))
)

def loopProps(g: Graph,e: Part,sprites: Sprites) = {
  val s: Part = g.subpart(src,e)
  val (spr: Sprite,sprops: PropMap) = sprites(s)
  val c: Complex = sprops.get(Center).getOrElse(Complex(0,0))
  val dir: Complex = Complex(0,1)

  val slot: Int = getSlot(g,e)
  val bend: Double = .1 + .05 * slot
  val rot: Complex = Complex(0,bend).exp

  val bs: Complex = spr.boundaryPt(sprops,-dir*rot)
  val bt: Complex = spr.boundaryPt(sprops,-dir*rot.cong)

  PropMap()
    .set(Loop,c)
    .set(Start,bs)
    .set(End,bt)
    .set(Bend,bend)
}

def edgeProps(g: Graph,e: Part,sprites: Sprites) = {
  val s: Part = g.subpart(src,e)
  val t: Part = g.subpart(tgt,e)
  val (sspr,sprops) = sprites(s)
  val (tspr,tprops) = sprites(t)
  val cs = sprops.get(Center).getOrElse(Complex(0,0))
  val ct = tprops.get(Center).getOrElse(Complex(10,10))
  val dir = if ct == cs 
    then Complex(1,0)
    else (ct - cs).normalize

  
  val slot = getSlot(g,e)
  val bend = .2 * slot
  val rot = Complex(0,bend).exp


  val bs = sspr.boundaryPt(sprops,dir*rot)
  val bt = tspr.boundaryPt(tprops,-dir*rot.cong)

  PropMap()
    .set(Start,bs)
    .set(End,bt)
    .set(Bend,bend)
}

def dragProps(g: Graph,e: Part,sprites: Sprites) = {
  val zs = g.trySubpart(Start,e).getOrElse(Complex(0,0))
  val zt = g.trySubpart(End,e).getOrElse(Complex(10,10))
  val dir = if zs == zt then Complex(1,0) else (zt-zs).normalize
  PropMap()
    .set(Start,zs)
    .set(End,zt - 10*dir)
    .set(Bend,0)
}

def getSlot(g:Graph,e: Part) = 
  (g.srcOf(e),g.tgtOf(e)) match
    case (Some(s),Some(t)) =>
      val idx = g.edgesFrom(s,t).toList.indexOf(e)
      val fwd = g.edgesFrom(s,t).size
      val bwd = g.edgesFrom(t,s).size
      if bwd == 0 then
        (1 - fwd) + 2 * idx
      else
        1 + 2 * idx    
    case _ => 0


  // Middleware (controllers & defaults)
def nodeMiddleware(
  hover:HoverController,
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() 
    + (Stroke,"black")
    + (Fill, "lightgray")
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)
    + (InnerSep, 7)
    + (FontSize, 16)
    + (Style, "margin:50px;")
  ),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(0.7)")
  ),
  Clickable(mouse, MainHandle)
)



def edgeMiddleware(
  hover:HoverController, 
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() + (Stroke,"black")),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(0.7)")
  ),
  Clickable(mouse, MainHandle)
)

// Sprite makers


def nodeMaker(
  hover:HoverController,
  mouse:MouseController
) = SpriteMaker[Graph](
  Box(),     // : Sprite       
  (g:Graph,_) => extractNodes(g),
  // middleware: Middleware
  nodeMiddleware(hover,mouse)
)



def edgeMaker(hover:HoverController,mouse:MouseController) = SpriteMaker[Graph](
  Arrow(),
  extractEdges,
  edgeMiddleware(hover,mouse)
)


// DOM object (svg)

def renderApp(
  $appModel: Var[Graph],
  hover: HoverController,
  mouse: MouseController
) = {
  val spriteMaps = SpriteMaps[Graph](
    $appModel.signal,                     // : Signal[Graph]
    List(
      nodeMaker(hover,mouse),
      edgeMaker(hover,mouse),
    )
  )
  
  svg.g(
    spriteMaps.attach
  )
}


//================ Main =====================

val serializer = ACSet.rw[GraphSchema]

object Main {

  // val g: Graph = buildGraph()

  

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- initialize(buildGraph())
      _ <- addRelative(renderApp($model,hover,mouse))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head")
      .appendChild(styleTag().ref)

    plutoMain(el,Graph(),serializer,action)
  }
}


























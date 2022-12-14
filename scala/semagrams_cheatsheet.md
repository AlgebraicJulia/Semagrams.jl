# Semagrams cheatsheet

## Model

- `e:Entity`
  - `e.entityType: EntityType`
  - `e.hash: Int`
  - `e.withType(ty: EntityType): Option[Entity]`
- `p: Property`
  - `p.Value` (internal property type)
  - `p.readValue(sv: ujson.Value)`
  - `p.writeValue(x:Any)`
  - Subtypes (`Value = T`)
    - `p:PValue[T : ReadWriter]`
    - `p: GenericProperty[T]`
      - `T = String`:
        - `Fill, Stroke, Content, Style`
      - `T = Double`
        -  `FontSize, Bend, MinimumSize, MinimumWidth, MinimumHeight`
      - `T = Complex`
        - `Center, Start, End`
- `m:PropMap(map:Map[Property,Any])`
  - `m = PropMap()`
  - `m.contains(p: Property): Boolean`
  - `m.apply(p::Property): p.Value`
  - `m.get(p: Property): Option[p.Value]`
  - `m.set(k: Property, v: k.Value): PropMap`
  - `m +/- (kv: (GenericProperty[T],T)): PropMap`
  - `m ++/-- (n: PropMap): PropMap`
  - `m.toJSon(): Map[String,ujason.Value]`


## ACSets

### Schemas

- `Ob <: EntityType`
- `Part(id:Int,ob:Ob) <: Entity`
- `<: Property`
  - `HomWithDom <: Hom, AttrWithDom <: Attr`
- `S: IsSchema`
  - `S = BasicSchema(gens: (Ob | (Ob,Hom) | (Ob,Attr) | HomWithDom | AttrWithDom))`
  - `S = DynSchema()`
  - `S.obs: Seq[Ob]`
  - `S.homs(x: Ob): Seq[Hom]`
  - `S.attrs(x: Ob): Seq[Attr]`
  - `S.obsByString: Map[String, Ob]`
  - `S.homsByString: Map[String, Hom]`
  - `S.attrsByString: Map[String,Attr]`
  - `S.contains(x: Ob): Boolean`
  - `S.contains(x: Ob. f: Hom): Boolean`
  - `S.contains(x: Ob, f: Attr): Boolean`

### ACSets


- `I: ACSet[S : IsSchema]`
  - `I = ACSet(s : S)`
  - `I.schema: S`
  - `I.counter: Int`
  - `I.parts: Map[Ob, Set[Part]]`
  - `I.props: Map[Part,PropMap]` 
  - `I.addPart(ob: Ob): (ACSet[S],Part)`
  - `I.addPart(ob:Ob, props: PropMap): (ACSet[S],Part)`
  - `I.remPart(x: Part): ACSet[S]`
  - `I.setSubparts(x: Part, pm: PropMap): ACSet[S]`
  - `I.setSubparts(pms: Iterable[(Part,PropMap)]): ACSet[S]`
  - `I.remSubpart(f: Property,x: Part): ACSet[S]`
  - `I.subpart(f: Property,x: Part): f.Value`
  - `I.trySubpart(f: Property,x: Part): Option[f.Value]`
  - `I.incident(f: Property,dom: Ob,y: f.Value): Set[Part]`
  - `I.incident(f: HomWithDom, y: Part): Set[Part]`
  - `I.incident(f: AttrWithDom, y: f.Value): Set[Part]`
  - `I.toSerializable: SerializableACSet`
  - 
## Actions

### EditorState

- `s:EditorState[Model]`
  - `s = EditorState($model: Var[Model],elt: SvgElement,update: () => Unit)`
  - Controllers: `s.mouse, s.drag, s.hover, s.keyboard, s.transform`
  - `s.bindables: EventBus[Any]`
  - `s.$model: Var[Model]`
  - `s.elt, s.playArea: SvgElement`
  - `s.controlChildCommands, s.relativeChildCommands: Observer[ChildrenCommand]`
  - `s.update: () => Unit`
  - `s.dims(): Complex`

## Actions

- `a: Action[Model,A]` (= `ReaderT[IO,EditorState[Model],A]`)
  - `a = Action(f: EditorState[Model] => IO[A])`
  - `a.toOption: Action[Model,Option[A]]`
  - `a.onCancelOrError(fin: Action[Model,A]): Action[Model,A]`
- `o = Action.ops[Model]: ActionOps[Model]`
  - `o.ask.map(f: EditorState[Model] => A): Action[Model,A]`
  - `o.ask.flatMap(f: EditorState[Model] => F[A]): Action[Model,F[A]]`
### Core Actions
  - `nextKeyDown: Action[Model,KeyboardEvent]`
  - `nextKey: Action[Model,String]`
  - `nextKeydownIn(set: Set[String]): Action[Model,KeyboardEvent]`
  - `updateModel(f: Model => Model): Action[Model,Unit]`
  - `updateModelS(updater: State[Model,A]): Action[Model,A]`
  - `getModel: Action[Model,Var[Model]]`
  - `mousePos: Action[Model,Complex]`
  - `mouseDown(b::MouseButton): Action[Model,Option[Entity]]`
  - `hovered: Action[Model,Option[Entity]]`
  - `hoveredEntity(x: EntityType): Action[Model,Option[Entity]]`
  - `getClick(x: EntityType): Action[Model,Entity]`
  - `update: Action[Model,Unit]`
  - `editText(listener:Observer[String],init:String):Action[Model,Unit]`
  - `delay(seconds: Double): Action[Model,Unit]`
  - `runUntil(a:Action[Model,Unit],condition: Action[Model,Unit]): Action[Model,Unit]`
  - `addRelative(child:SvgElement): Action[Model,Unit]`
  - `addControl(child:SvgElement): Action[Model,Unit]`
  - `removeControl(child:SvgElement): Action[Model,Unit]`
  - `zoomBy(factor:Double): Action[Model,Unit]`
  - `zoomAtMouse(factor: Double): Action[Model,Unit]`
  - `dragPan: Action[Model,Unit]`

### ACSet Actions

- `ao = summon[ACSetOps[S : IsSchema]]`
  - `ao.addPart(ob: Ob): State[ACSet[S],Part]`
  - `ao.addPart(ob: Ob, props: PropMap): State[ACSet[S],Part]`
  - `ao.addPartPos(ob: Ob, props:PropMap): State[ACSet[S],Part]`
  - `ao.setSubpart(f: Property, x: Part, y: f.Value): State[ACSet[S],Unit]`
  - `ao.remPart(x: Part): State[ACSet[S],Unit]`
  - `hoveredPart(ob: Ob): Action[ACSet[S],Option[Entity]]`
  - `ao.editStringProp(attr:Property)(v: Part): Action[ACSet[S],Unit]`
  - `dragEdge(ob:Ob,src:Hom,tgt:Hom,s:Part): Action[ACSet[S],Unit]`
  - `dragPart(v:Part): Action[ACSet[S],Unit]`
  - `ao.subPartLens(f: Attr, x: Part): Lens[ACSet[S], f.Value]`

## Bindings

- `b: Binding[Model,A]`
  - `b = Binding(f: PartialFunction[Any,Action[Model,A]])`
  - `b.f: PartialFunction[Any,Action[Model,A]]`
  - `b.modifiers:Option[Set[KeyModifier]]`
  - `b.flatMap(g: A => Action[Model,B]): Binding[Model,B]`
  - `b.map(g: A => B): Binding[Model,B]`
  - `b.mapTo(b:B): Binding[Model,B]`
  - `andThen(mb: Action[Model,B]): Binding[Model,B]`
  - `b.fail: Binding[Model,Unit]`
  - `b.withMods(newMods:KeyModifiers*): Binding[Model,A]`

### Interaction enums
  - `Single, Double: ClickType`
  - `Left, Middle, Right: MouseButton`

### Core bindings

  - `keydown(key:String): Binding[Model,Unit]`
  - `keyUp(key:String): Binding[Model,Unit]`
  - `clickOn(ct:ClickType,b:MouseButton,x:EntityType):Binding[Model,Entity]`
  - `clickOnPart(ct:ClickType,b:MouseButton):Binding[Model,Part]`
  - `releaseOn(ct:ClickType,b:MouseButton,x: Ob): Binding[Model,Entity]`
  - `mouseMove:Binding[Model,Complex]`
  - `mouseUp(b:MouseButton):Binding[Model,Option[Entity]]`
  - `mouseLeave: Binding[Model,Complex]`

### Binding Actions
  - `showPopoverUntil(lines:Seq[String],binding:Binding[Model,Unit]): Action[Model,Unit]`
  - 


## Sprites

- `s: Sprite`
  - `s.present(e: Entity,init:PropMap,updates:Signal[PropMap]): RenderedSprite`
  - `s.boundaryPt(data: PropMap,dir: Complex): Complex`
- `rs: RenderedSprite`
  - `rs.root: SvgElement`
  - `rs.handles: Map[Handle, SvgElement]`
- `Sprites = Map[Entity,(Sprite,PropMap)]`
- `sm:SpriteMaker[State]`
  - `sm = SpriteMaker(sprite,extractor,middleware)`
  - `sm.sprite: Sprite`
  - `sm.extractor: (State,Sprites) => List[(Entity,PropMap)]`
  - `sm.middleWare: MiddleWare`
- `sms:SpriteMaps[State]`
  - `sms.$state: Signal[State]`
  - `sms.spriteMakers: List[SpriteMaker[State]]`
  - `sms.$sprites: Signal[(List[Sprites],Sprites)]`

## Middleware

- `m: Middleware`
  - `m.updateProps(e: Entity,p: PropMap): PropMap`
  - `m.updatePropsS(e: Entity, $p: Signal[PropMap]): Signal[PropMap]`
  - `m.modifyRendered(e: Entity, s: RenderedSprite): RenderedSprite`
- `s: Stack <: Middleware`
  - `s = Stack(mws: Middleware*)`
  - `s.ms: List[Middleware]`
- `c: Clickable <: Middleware`
  - `c.mouse: MouseController`
  - `c.handle: Handle`
- `h: Hoverable <: Middleware`
  - `h.hover: HoverController`
  - `h.handle: Handle`
  - `h.extraProps: PropMap`


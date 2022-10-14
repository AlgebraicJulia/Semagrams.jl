package semagrams.acsets

case class DynOb(name: String) extends Ob

case class DynHom(name: String, dom: DynOb, codom: DynOb) extends AbstractHom

case class DynAttrType(name: String) extends AttrType

case class DynAttr(name: String, dom: DynOb, codom: DynAttrType) extends AbstractAttr

case class DynamicACSet(schema: Schema, bare: BareACSet) {

}

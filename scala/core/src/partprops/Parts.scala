package semagrams

import upickle.default._

import semagrams.util._
  
case class Part(id:UID,ob:Ob) extends Entity:

  val ty = ob

  override def toString = if ob.label == ""
    then "Anon-" + id.rand
    else ob.label + "-" + id.rand


  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.label + id.toString


object Part:
  def apply(x:Ob) = new Part(UID("Part"),x)
  def apply(id:UID,x:Ob) = new Part(id,x)
  
  
  def rw: ReadWriter[Part] = 
    readwriter[(UID,UID)].bimap[Part](
      part => (part.ob.id,part.id),
      (obId,partId) => Part(partId,Table(obId))
    )





/** A trait for the representation of individual parts in an ACSet, defining
 *  interoperability with `PropMap`s 
 */
trait PartData[Data]:
  def fromProps(props:PropMap): Data
  extension (d:Data)
    /* Implementation API */

    /* Get all available properties */
    def getProps(): PropMap

    /* Set a single property */
    def setProp(f:Property,v:f.Value): Data

    /* Remove a single property */
    def remProp(f:Property): Data


    /* Generic methods */

    /* Getting */

    /* Check for property `f` */
    def hasProp(f:Property): Boolean = d.getProps().contains(f)
    def contains(fs:Property*): Boolean = fs.forall(d.hasProp(_))
    def hasProps(fs:Iterable[Property]): Boolean = d.getProps().contains(fs.toSeq:_*)

    /* Return `Some(v)` if `f` is set and `None` otherwise */
    def tryProp(f:Property): Option[f.Value] = d.getProps().get(f)

    /* Return the assigned property value (unsafe) */
    def getProp(f:Property): f.Value = d.getProps()(f)    
    
    /* Setting */

    /* Set a single property (f -> v) */
    def +[T](kv:(Property{type Value = T},T)): Data = d.setProp(kv._1,kv._2)

    /* Set a family of properties `props` */
    def setProps(props:PropMap): Data =
      props.pmap.keys.toSeq match
        case Seq() => d
        case f +: rest => d.setProp(f,props(f)).setProps(props - f)

    /* Merge the properties of `d2` into `d`, overwriting on collisions */
    def merge[Data2:PartData](d2:Data2) = d.setProps(d2.getProps())

    /* Alias for `merge`. Overwrites on collisions */
    def ++[Data2:PartData](d2:Data2) = d.merge(d2)

    /* Remove a family of properties `props` */
    def remProps(props:Iterable[Property]): Data = props.toSeq match
      case Seq() => d
      case f +: rest => d.remProp(f).remProps(rest)
    

    /* Remove the properties in `d2` from those in `d`. */
    def diff[Data2:PartData](d2:Data2) = d.setProps(d2.getProps())


    /* Optionally set `f` to `v`, conditional on pred */
    def optionalSet(f:Property,pred:Option[f.Value] => Boolean,v:f.Value) =
      if pred(d.tryProp(f))
      then d.setProp(f,v)
      else d


    /* Optionally set `f` to `v`, conditional on pred or if the value is missing */
    def conditionalSet(f:Property,pred:f.Value => Boolean,v:f.Value) = d.tryProp(f) match
      case Some(v0) => if pred(v0) then d.setProp(f,v) else d
      case None => d.setProp(f,v)

    /* Set `f` to `v` if it is not set */
    def softSetProp(f:Property,v:f.Value): Data =
      d.conditionalSet(f,_=>false,v)

    /* Set the properties `props` if they are not set */
    def softSetProps(props:PropMap): Data =
      d.setProps(
        props.filterKeys(!hasProp(_))
      )
    
    def softMerge[Data2:PartData](d2:Data2) = d.softSetProps(d2.getProps())


    /* Collect all schema generators from `FKey` values */
    def generators() = d.getProps().keySeq.flatMap{
      case e:Elt => e.generators
      case _ => Seq()
    }.toMap


    
object PartData:
  
  def apply[D:PartData](props:PropMap = PropMap()) =
    val pd = summon[PartData[D]] 
    pd.fromProps(props)

  implicit val propsAreData:PartData[PropMap] = new PartData[PropMap] {
    def fromProps(props:PropMap) = props
    extension (props:PropMap)
      def getProps() = props
      def setProp(f:Property,v:f.Value) = props + (f,v)
      def remProp(f:Property) = props - f
      def merge(that:PropMap) = props ++ that
  }



package semagrams

import semagrams.util.Complex
import scala.annotation.targetName

/** A dependently-typed, persistent mapping from properties to values; the type
  * of the value stored for a given property depends on the property.
  *
  * We implement this internally with a `Map` from [[Property]] to `Any`, and
  * then enforce that only things of the right property are used by only
  * exposing well-typed insertion mappings.
  */
case class PropMap(pmap: Map[Property, Any]) {

  /** Fetch the value corresponding to `p`, throwing if `p` not found */
  def apply(p: Property): p.Value = {
    pmap(p).asInstanceOf[p.Value]
  }

  /** Fetch the value corresponding to `p`, returning None if `p` not found */
  def get(p: Property): Option[p.Value] = {
    pmap.get(p).map(_.asInstanceOf[p.Value])
  }

  /** Returns a new PropMap with `k` set to `v` */
  def set(k: Property, v: k.Value): PropMap = {
    this.copy(pmap = pmap + (k -> v.asInstanceOf[Any]))
  }

  /** Removes property `k` from the new PropMap */
  def rem(k: Property): PropMap = this - k

  /** Returns a new PropMap with `kv(1)` set to `kv(2)`
    *
    * Scala's type system doesn't have Sigma-types, so this only works with
    * [[GenericProperty]], not [[Property]]
    */
  def +[T](kv: (Property{ type Value = T}, T)): PropMap = {
    val (k, v) = kv
    this.copy(pmap = pmap + (k.asInstanceOf[Property] -> v.asInstanceOf[Any]))
  }

  def keySeq = pmap.keys.toSeq

  def filterKeys(p: Property => Boolean) = PropMap(
    pmap.view.filterKeys(p).toMap
  )

  def filter(pred:(Property,Any) => Boolean) = PropMap(
    pmap.filter(pred.tupled)
  )

  def transform(prop:Property,f:prop.Value => prop.Value) = get(prop) match
    case Some(v) => set(prop,f(v))
    case None => this

  def scale(f:Property{type Value = Complex},from:Complex,to:Complex) =
    get(f) match
      case Some(v) => set(f,(v/from)*to)
      case None => this
    
  def scale(fs:Seq[Property{type Value = Complex}],from:Complex,to:Complex): PropMap =
    fs match
      case Seq() => this
      case head +: tail => scale(head,from,to).scale(tail,from,to)
    

  


  /** Returns a new PropMap given by overwriting `this` with the key-value pairs
    * in `other`.
    */
  def ++(other: PropMap): PropMap = {
    this.copy(pmap = pmap ++ other.pmap)
  }

  /** Use the serializers in the properties to write this out. */
  def toJson(): Map[String, ujson.Value] =
    pmap.map((k, v) => (k.toString, k.writeValue(v)))

  /** Unset all of the properties in `ps` */
  def --(ps: IterableOnce[Property]) = PropMap(pmap -- ps)

  /** Unset `p` */
  def -(p: Property): PropMap = this -- Seq(p)

  /** Check if `p` is set */
  def contains(p: Property):Boolean = pmap contains p

  /** Check if `ps` are set */
  def contains(ps: Seq[Property]): Boolean = ps.forall(p => contains(p))
}

object PropMap {

  /** Construct a new empty PropMap */
  def apply() = {
    new PropMap(Map[Property, Any]())
  }

  def apply(kvs:(Property,Any)*) = new PropMap(kvs.toMap)

  @targetName("PropMapConvenienceConstructor")
  def apply(pvals:PropVal[_]*) = new PropMap(
    pvals.flatMap(pval =>
      pval.value.map(v => pval.prop -> v)
    ).toMap
  )
  /** Deserialize a PropMap, using a supplied map which says how to interpret
    * strings as properties
    */
  def fromJson(
      usingProps: Map[String, Property],
      serialized: Map[String, ujson.Value]
  ) =
    new PropMap(
      serialized.map((sk, sv) => {
        val k = usingProps(sk)
        (k, k.readValue(sv))
      })
    )
}

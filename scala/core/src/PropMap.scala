package semagrams

/** A dependently-typed, persistent mapping from properties to values; the type
  * of the value stored for a given property depends on the property.
  *
  * We implement this internally with a `Map` from [[Property]] to `Any`, and
  * then enforce that only things of the right property are used by only
  * exposing well-typed insertion mappings.
  */
case class PropMap(map: Map[Property, Any]) {

  /** Fetch the value corresponding to `p`, throwing if `p` not found */
  def apply(p: Property): p.Value = {
    map(p).asInstanceOf[p.Value]
  }

  /** Fetch the value corresponding to `p`, returning None if `p` not found */
  def get(p: Property): Option[p.Value] = {
    map.get(p).map(_.asInstanceOf[p.Value])
  }

  /** Returns a new PropMap with `k` set to `v` */
  def set(k: Property, v: k.Value): PropMap = {
    this.copy(map = map + (k -> v.asInstanceOf[Any]))
  }

  /** Returns a new PropMap with `kv(1)` set to `kv(2)`
    *
    * Scala's type system doesn't have Sigma-types, so this only works with
    * [[GenericProperty]], not [[Property]]
    */
  def +[T](kv: (GenericProperty[T], T)): PropMap = {
    val (k, v) = kv
    this.copy(map = map + (k.asInstanceOf[Property] -> v.asInstanceOf[Any]))
  }

  /** Returns a new PropMap given by overwriting `this` with the key-value pairs
    * in `other`.
    */
  def ++(other: PropMap): PropMap = {
    this.copy(map = map ++ other.map)
  }

  /** Use the serializers in the properties to write this out. */
  def toJson(): Map[String, ujson.Value] =
    map.map((k, v) => (k.toString, k.writeValue(v)))

  /** Unset all of the properties in `ps` */
  def --(ps: IterableOnce[Property]) = PropMap(map -- ps)

  /** Unset `p` */
  def -(p: Property): PropMap = this -- Seq(p)

  /** Check if `p` is set */
  def contains(p: Property) = map contains p
}

object PropMap {

  /** Construct a new empty PropMap */
  def apply() = {
    new PropMap(Map[Property, Any]())
  }

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

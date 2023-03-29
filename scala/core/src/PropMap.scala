package semagrams

case class PropMap(map: Map[Property, Any]) {
  def apply(p: Property): p.Value = {
    map(p).asInstanceOf[p.Value]
  }

  def get(p: Property): Option[p.Value] = {
    map.get(p).map(_.asInstanceOf[p.Value])
  }

  def set(k: Property, v: k.Value): PropMap = {
    this.copy(map = map + (k -> v.asInstanceOf[Any]))
  }

  def +[T](kv: (GenericProperty[T], T)) = {
    val (k, v) = kv
    this.copy(map = map + (k.asInstanceOf[Property] -> v.asInstanceOf[Any]))
  }

  def ++(other: PropMap): PropMap = {
    this.copy(map = map ++ other.map)
  }

  def toJson(): Map[String, ujson.Value] =
    map.map((k, v) => (k.toString, k.writeValue(v)))

  def --(ps: IterableOnce[Property]) = PropMap(map -- ps)

  def -(p: Property): PropMap = this -- Seq(p)

  def contains(p: Property) = map contains p
}

object PropMap {
  def apply() = {
    new PropMap(Map[Property, Any]())
  }

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

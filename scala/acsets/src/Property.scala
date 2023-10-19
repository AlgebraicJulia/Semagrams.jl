package acsets

trait Property {
  val id: PropId
  val valueType: ValueType

  type T = valueType.T

  def produce(x: T): Value = valueType.produce(x)
  def coerce(v: Value): Option[T] = valueType.coerce(v)
}

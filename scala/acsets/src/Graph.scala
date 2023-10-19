package acsets

val V = SortId("V")
val E = SortId("E")

case object src extends Property {
  val id = PropId("src")
  val valueType: ValueType.Reference = ValueType.Reference(V)
}

case object tgt extends Property {
  val id = PropId("tgt")
  val valueType: ValueType.Reference = ValueType.Reference(V)
}

val SchGraph = Schema
  .Dirty()
  .addSort(V)
  .addSort(E)
  .addProp(src)
  .addProp(tgt)
  .addMethod(E, src)
  .addMethod(E, tgt)
  .applyPatch()
  .get

case object intWeight extends Property {
  val id = PropId("weight")
  val valueType: ValueType.I.type = ValueType.I
}

val SchWeightedGraph = Schema
  .Dirty(SchGraph)
  .addProp(intWeight)
  .addMethod(E, intWeight)
  .applyPatch()
  .get

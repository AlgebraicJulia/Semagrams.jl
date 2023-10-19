package acsets

val V = SortId("V")
val E = SortId("E")

val src = PropId("src")
val tgt = PropId("tgt")

val SchGraph = Schema
  .Dirty()
  .addSort(V)
  .addSort(E)
  .addProp(src, ValueType.Reference(V))
  .addProp(tgt, ValueType.Reference(V))
  .addMethod(E, src)
  .addMethod(E, tgt)
  .applyPatch()
  .get
